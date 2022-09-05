{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, FlexibleContexts, ScopedTypeVariables #-}
{- |

Module      :  Network.Browser
Copyright   :  See LICENSE file
License     :  BSD

Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
Stability   :  experimental
Portability :  non-portable (not tested)

Session-level interactions over HTTP.

The "Network.Browser" goes beyond the basic "Network.HTTP" functionality in
providing support for more involved, and real, request/response interactions over
HTTP. Additional features supported are:

* HTTP Authentication handling

* Transparent handling of redirects

* Cookie stores + transmission.

* Transaction logging

* Proxy-mediated connections.

Example use:

>    do
>      (_, rsp)
>         <- Network.Browser.browse $ do
>               setAllowRedirects True -- handle HTTP redirects
>               request $ getRequest "http://www.haskell.org/"
>      return (take 100 (rspBody rsp))

-}
module Network.Browser
       ( BrowserState
       , BrowserAction      -- browser monad, effectively a state monad.
       , Proxy(..)

       , browse             -- :: BrowserAction a -> IO a
       , request            -- :: Request -> BrowserAction Response

       , getBrowserState    -- :: BrowserAction (BrowserState t)
       , withBrowserState   -- :: BrowserState t -> BrowserAction a -> BrowserAction a

       , setAllowRedirects  -- :: Bool -> BrowserAction ()
       , getAllowRedirects  -- :: BrowserAction Bool

       , setMaxRedirects    -- :: Int -> BrowserAction ()
       , getMaxRedirects    -- :: BrowserAction (Maybe Int)

       , Authority(..)
       , getAuthorities
       , setAuthorities
       , addAuthority
       , Challenge(..)
       , Qop(..)
       , Algorithm(..)

       , getAuthorityGen
       , setAuthorityGen
       , setAllowBasicAuth
       , getAllowBasicAuth

       , setMaxErrorRetries  -- :: Maybe Int -> BrowserAction ()
       , getMaxErrorRetries  -- :: BrowserAction (Maybe Int)

       , setMaxPoolSize     -- :: Int -> BrowserAction ()
       , getMaxPoolSize     -- :: BrowserAction (Maybe Int)

       , setMaxAuthAttempts  -- :: Maybe Int -> BrowserAction ()
       , getMaxAuthAttempts  -- :: BrowserAction (Maybe Int)

       , setCookieFilter     -- :: (URI -> Cookie -> IO Bool) -> BrowserAction ()
       , getCookieFilter     -- :: BrowserAction (URI -> Cookie -> IO Bool)
       , defaultCookieFilter -- :: URI -> Cookie -> IO Bool
       , userCookieFilter    -- :: URI -> Cookie -> IO Bool

       , Cookie(..)
       , getCookies        -- :: BrowserAction [Cookie]
       , setCookies        -- :: [Cookie] -> BrowserAction ()
       , addCookie         -- :: Cookie   -> BrowserAction ()

       , setErrHandler     -- :: (String -> IO ()) -> BrowserAction ()
       , setOutHandler     -- :: (String -> IO ()) -> BrowserAction ()

       , setEventHandler   -- :: (BrowserEvent -> BrowserAction ()) -> BrowserAction ()

       , BrowserEvent(..)
       , BrowserEventType(..)
       , RequestID

       , setProxy         -- :: Proxy -> BrowserAction ()
       , getProxy         -- :: BrowserAction Proxy

       , setCheckForProxy -- :: Bool -> BrowserAction ()
       , getCheckForProxy -- :: BrowserAction Bool

       , getUserAgent     -- :: BrowserAction String
       , setUserAgent     -- :: String -> BrowserAction ()

       , out              -- :: String -> BrowserAction ()
       , err              -- :: String -> BrowserAction ()

       , defaultGETRequest

       , formToRequest
       , uriDefaultTo

         -- old and half-baked; don't use:
       , Form(..)
       , FormVar
       ) where

import Network.URI
   ( URI(..)
   , URIAuth(..)
   , parseURI, parseURIReference, relativeTo
   , escapeURIString, isUnescapedInURI
   )
import Network.HTTP hiding ( sendHTTP_notify, getCookies, setCookies )
import Network.HTTP.HandleStream ( sendHTTP_notify )
import Network.HTTP.Auth
import Network.HTTP.Cookie
import Network.HTTP.Proxy
import Network.HTTP.Utils ( HttpError(..) )
import Network.URI ( uriAuthToString )

#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail
#endif

import Data.Char (toLower)
import Data.List (partition, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes )
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif
import Control.Exception ( Exception(..), SomeException, catch, throwIO )
import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class
   ( MonadIO (..) )
import Control.Monad.State
   ( MonadState(..), gets, modify, StateT (..), evalStateT, withStateT )

import qualified System.IO
   ( hSetBuffering, hPutStr, stdout, stdin, hGetChar
   , BufferMode(NoBuffering, LineBuffering)
   )
import Data.Time.Clock ( UTCTime, getCurrentTime )


------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @defaultCookieFilter@ is the initial cookie acceptance filter.
-- It welcomes them all into the store @:-)@
defaultCookieFilter :: URI -> Cookie -> IO Bool
defaultCookieFilter _url _cky = return True

-- | @userCookieFilter@ is a handy acceptance filter, asking the
-- user if he/she is willing to accept an incoming cookie before
-- adding it to the store.
userCookieFilter :: URI -> Cookie -> IO Bool
userCookieFilter url cky = do
    do putStrLn ("Set-Cookie received when requesting: " ++ show url)
       case ckComment cky of
          Nothing -> return ()
          Just x  -> putStrLn ("Cookie Comment:\n" ++ x)
       let pth = maybe "" ('/':) (ckPath cky)
       putStrLn ("Domain/Path: " ++ ckDomain cky ++ pth)
       putStrLn (ckName cky ++ '=' : ckValue cky)
       System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
       System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
       System.IO.hPutStr System.IO.stdout "Accept [y/n]? "
       x <- System.IO.hGetChar System.IO.stdin
       System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
       System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
       return (toLower x == 'y')

-- | @addCookie c@ adds a cookie to the browser state, removing duplicates.
addCookie :: Cookie -> BrowserAction ()
addCookie c = modify (\b -> b{bsCookies = c : filter (/=c) (bsCookies b) })

-- | @setCookies cookies@ replaces the set of cookies known to
-- the browser to @cookies@. Useful when wanting to restore cookies
-- used across 'browse' invocations.
setCookies :: [Cookie] -> BrowserAction ()
setCookies cs = modify (\b -> b { bsCookies=cs })

-- | @getCookies@ returns the current set of cookies known to
-- the browser.
getCookies :: BrowserAction [Cookie]
getCookies = gets bsCookies

-- ...get domain specific cookies...
-- ... this needs changing for consistency with rfc2109...
-- ... currently too broad.
getCookiesFor :: String -> String -> BrowserAction [Cookie]
getCookiesFor dom path =
    do cks <- getCookies
       return (filter cookiematch cks)
    where
        cookiematch :: Cookie -> Bool
        cookiematch = cookieMatch (dom,path)


-- | @setCookieFilter fn@ sets the cookie acceptance filter to @fn@.
setCookieFilter :: (URI -> Cookie -> IO Bool) -> BrowserAction ()
setCookieFilter f = modify (\b -> b { bsCookieFilter=f })

-- | @getCookieFilter@ returns the current cookie acceptance filter.
getCookieFilter :: BrowserAction (URI -> Cookie -> IO Bool)
getCookieFilter = gets bsCookieFilter

------------------------------------------------------------------
----------------------- Authorisation Stuff ----------------------
------------------------------------------------------------------

{-

The browser handles 401 responses in the following manner:
  1) extract all WWW-Authenticate headers from a 401 response
  2) rewrite each as a Challenge object, using "headerToChallenge"
  3) pick a challenge to respond to, usually the strongest
     challenge understood by the client, using "pickChallenge"
  4) generate a username/password combination using the browsers
     "bsAuthorityGen" function (the default behaviour is to ask
     the user)
  5) build an Authority object based upon the challenge and user
     data, store this new Authority in the browser state
  6) convert the Authority to a request header and add this
     to a request using "withAuthority"
  7) send the amended request

Note that by default requests are annotated with authority headers
before the first sending, based upon previously generated Authority
objects (which contain domain information).  Once a specific authority
is added to a rejected request this predictive annotation is suppressed.

407 responses are handled in a similar manner, except
   a) Authorities are not collected, only a single proxy authority
      is kept by the browser
   b) If the proxy used by the browser (type Proxy) is NoProxy, then
      a 407 response will generate output on the "err" stream and
      the response will be returned.


Notes:
 - digest authentication so far ignores qop, so fails to authenticate
   properly with qop=auth-int challenges
 - calculates a1 more than necessary
 - doesn't reverse authenticate
 - doesn't properly receive AuthenticationInfo headers, so fails
   to use next-nonce etc

-}

-- | Return authorities for a given domain and path.
-- Assumes "dom" is lower case
getAuthFor :: String -> String -> BrowserAction [Authority]
getAuthFor dom pth = getAuthorities >>= return . (filter match)
   where
    match :: Authority -> Bool
    match au@AuthBasic{}  = matchURI (auSite au)
    match au@AuthDigest{} = or (map matchURI (auDomain au))

    matchURI :: URI -> Bool
    matchURI uri = (uriAuthToString id (uriAuthority uri) "" == dom) && (uriPath uri `isPrefixOf` pth)


-- | @getAuthorities@ return the current set of @Authority@s known
-- to the browser.
getAuthorities :: BrowserAction [Authority]
getAuthorities = gets bsAuthorities

-- @setAuthorities as@ replaces the Browser's known set
-- of 'Authority's to @as@.
setAuthorities :: [Authority] -> BrowserAction ()
setAuthorities as = modify (\b -> b { bsAuthorities=as })

-- @addAuthority a@ adds 'Authority' @a@ to the Browser's
-- set of known authorities.
addAuthority :: Authority -> BrowserAction ()
addAuthority a = modify (\b -> b { bsAuthorities=a:bsAuthorities b })

-- | @getAuthorityGen@ returns the current authority generator
getAuthorityGen :: BrowserAction (URI -> String -> IO (Maybe (String,String)))
getAuthorityGen = gets bsAuthorityGen

-- | @setAuthorityGen genAct@ sets the auth generator to @genAct@.
setAuthorityGen :: (URI -> String -> IO (Maybe (String,String))) -> BrowserAction ()
setAuthorityGen f = modify (\b -> b { bsAuthorityGen=f })

-- | @setAllowBasicAuth onOff@ enables\/disables HTTP Basic Authentication.
setAllowBasicAuth :: Bool -> BrowserAction ()
setAllowBasicAuth ba = modify (\b -> b { bsAllowBasicAuth=ba })

getAllowBasicAuth :: BrowserAction Bool
getAllowBasicAuth = gets bsAllowBasicAuth

-- | @setMaxAuthAttempts mbMax@ sets the maximum number of authentication attempts
-- to do. If @Nothing@, revert to default max.
setMaxAuthAttempts :: Maybe Int -> BrowserAction ()
setMaxAuthAttempts mb
 | fromMaybe 0 mb < 0 = return ()
 | otherwise          = modify (\ b -> b{bsMaxAuthAttempts=mb})

-- | @getMaxAuthAttempts@ returns the current max auth attempts. If @Nothing@,
-- the browser's default is used.
getMaxAuthAttempts :: BrowserAction (Maybe Int)
getMaxAuthAttempts = gets bsMaxAuthAttempts

-- | @setMaxErrorRetries mbMax@ sets the maximum number of attempts at
-- transmitting a request. If @Nothing@, rever to default max.
setMaxErrorRetries :: Maybe Int -> BrowserAction ()
setMaxErrorRetries mb
 | fromMaybe 0 mb < 0 = return ()
 | otherwise          = modify (\ b -> b{bsMaxErrorRetries=mb})

-- | @getMaxErrorRetries@ returns the current max number of error retries.
getMaxErrorRetries :: BrowserAction (Maybe Int)
getMaxErrorRetries = gets bsMaxErrorRetries

-- TO BE CHANGED!!!
pickChallenge :: Bool -> [Challenge] -> Maybe Challenge
pickChallenge allowBasic []
 | allowBasic = Just (ChalBasic "/") -- manufacture a challenge if one missing; more robust.
pickChallenge _ ls = listToMaybe ls

-- | Retrieve a likely looking authority for a Request.
anticipateChallenge :: Request -> BrowserAction (Maybe Authority)
anticipateChallenge rq = do
  let uri = rqURI rq
  auth <- getAuth rq
  authlist <- getAuthFor (uriAuthToString id (Just auth) "") (uriPath uri)
  return (listToMaybe authlist)

-- | Asking the user to respond to a challenge
challengeToAuthority :: URI -> Challenge -> BrowserAction (Maybe Authority)
challengeToAuthority uri ch
 | not (answerable ch) = return Nothing
 | otherwise = do
      -- prompt user for authority
    prompt <- getAuthorityGen
    userdetails <- liftIO $ prompt uri (chRealm ch)
    case userdetails of
     Nothing    -> return Nothing
     Just (u,p) -> return (Just $ buildAuth ch u p)
 where
  answerable :: Challenge -> Bool
  answerable ChalBasic{} = True
  answerable chall       = (chAlgorithm chall) == Just AlgMD5

  buildAuth :: Challenge -> String -> String -> Authority
  buildAuth (ChalBasic r) u p =
       AuthBasic { auSite=uri
                 , auRealm=r
                 , auUsername=u
                 , auPassword=p
                 }

    -- note to self: this is a pretty stupid operation
    -- to perform isn't it? ChalX and AuthX are so very
    -- similar.
  buildAuth (ChalDigest r d n o _stale a q) u p =
            AuthDigest { auRealm=r
                       , auUsername=u
                       , auPassword=p
                       , auDomain=d
                       , auNonce=n
                       , auOpaque=o
                       , auAlgorithm=a
                       , auQop=q
                       }


------------------------------------------------------------------
------------------ Browser State Actions -------------------------
------------------------------------------------------------------


-- | @BrowserState@ is the (large) record type tracking the current
-- settings of the browser.
data BrowserState
 = BS { bsErr, bsOut      :: String -> IO ()
      , bsCookies         :: [Cookie]
      , bsCookieFilter    :: URI -> Cookie -> IO Bool
      , bsAuthorityGen    :: URI -> String -> IO (Maybe (String,String))
      , bsAuthorities     :: [Authority]
      , bsAllowRedirects  :: Bool
      , bsAllowBasicAuth  :: Bool
      , bsMaxRedirects    :: Maybe Int
      , bsMaxErrorRetries :: Maybe Int
      , bsMaxAuthAttempts :: Maybe Int
      , bsMaxPoolSize     :: Maybe Int
      , bsConnectionPool  :: [Connection]
      , bsCheckProxy      :: Bool
      , bsProxy           :: Proxy
      , bsEvent           :: Maybe (BrowserEvent -> BrowserAction ())
      , bsRequestID       :: RequestID
      , bsUserAgent       :: Maybe String
      }

instance Show BrowserState where
    show bs =  "BrowserState { "
            ++ shows (bsCookies bs) ("\n"
           {- ++ show (bsAuthorities bs) ++ "\n"-}
            ++ "AllowRedirects: " ++ shows (bsAllowRedirects bs) "} ")

-- | @BrowserAction@ is the IO monad, but carrying along a 'BrowserState'.
newtype BrowserAction a
 = BA { unBA :: StateT BrowserState IO a }
 deriving
 ( Functor, Applicative, Monad, MonadIO, MonadState BrowserState
#if MIN_VERSION_base(4,9,0)
 , MonadFail
#endif
 )

runBA :: BrowserState -> BrowserAction a -> IO a
runBA bs = flip evalStateT bs . unBA

-- | @browse act@ is the toplevel action to perform a 'BrowserAction'.
-- Example use: @browse (request (getRequest yourURL))@.
browse :: BrowserAction a -> IO a
browse = runBA defaultBrowserState

-- | The default browser state has the settings
defaultBrowserState :: BrowserState
defaultBrowserState = BS
     { bsErr              = putStrLn
     , bsOut              = putStrLn
     , bsCookies          = []
     , bsCookieFilter     = defaultCookieFilter
     , bsAuthorityGen     = \ _uri _realm -> do
          putStrLn "No action for prompting/generating user+password credentials provided (use: setAuthorityGen); returning Nothing"
          return Nothing
     , bsAuthorities      = []
     , bsAllowRedirects   = True
     , bsAllowBasicAuth   = False
     , bsMaxRedirects     = Nothing
     , bsMaxErrorRetries  = Nothing
     , bsMaxAuthAttempts  = Nothing
     , bsMaxPoolSize      = Nothing
     , bsConnectionPool   = []
     , bsCheckProxy       = defaultAutoProxyDetect
     , bsProxy            = noProxy
     , bsEvent            = Nothing
     , bsRequestID        = 0
     , bsUserAgent        = Nothing
     }

{-# DEPRECATED getBrowserState "Use Control.Monad.State.get instead." #-}
-- | @getBrowserState@ returns the current browser config. Useful
-- for restoring state across 'BrowserAction's.
getBrowserState :: BrowserAction BrowserState
getBrowserState = get

-- | @withBrowserAction st act@ performs @act@ with 'BrowserState' @st@.
withBrowserState :: BrowserState -> BrowserAction a -> BrowserAction a
withBrowserState bs = BA . withStateT (const bs) . unBA

-- | @nextRequest act@ performs the browser action @act@ as
-- the next request, i.e., setting up a new request context
-- before doing so.
nextRequest :: BrowserAction a -> BrowserAction a
nextRequest act = do
  let updReqID st =
       let
        rid = succ (bsRequestID st)
       in
       rid `seq` st{bsRequestID=rid}
  modify updReqID
  act

catchBrowseException :: Exception e => BrowserAction a -> (e -> BrowserAction a) -> BrowserAction a
catchBrowseException f g = 
  BA (StateT (\st -> catch (runStateT (unBA f) st) (\e ->  runStateT (unBA (g e)) st)))

-- | @setErrHandler@ sets the IO action to call when
-- the browser reports running errors. To disable any
-- such, set it to @const (return ())@.
setErrHandler :: (String -> IO ()) -> BrowserAction ()
setErrHandler h = modify (\b -> b { bsErr=h })

-- | @setOutHandler@ sets the IO action to call when
-- the browser chatters info on its running. To disable any
-- such, set it to @const (return ())@.
setOutHandler :: (String -> IO ()) -> BrowserAction ()
setOutHandler h = modify (\b -> b { bsOut=h })

out, err :: String -> BrowserAction ()
out s = do { f <- gets bsOut ; liftIO $ f s }
err s = do { f <- gets bsErr ; liftIO $ f s }

-- | @setAllowRedirects onOff@ toggles the willingness to
-- follow redirects (HTTP responses with 3xx status codes).
setAllowRedirects :: Bool -> BrowserAction ()
setAllowRedirects bl = modify (\b -> b {bsAllowRedirects=bl})

-- | @getAllowRedirects@ returns current setting of the do-chase-redirects flag.
getAllowRedirects :: BrowserAction Bool
getAllowRedirects = gets bsAllowRedirects

-- | @setMaxRedirects maxCount@ sets the maximum number of forwarding hops
-- we are willing to jump through. A no-op if the count is negative; if zero,
-- the max is set to whatever default applies. Notice that setting the max
-- redirects count does /not/ enable following of redirects itself; use
-- 'setAllowRedirects' to do so.
setMaxRedirects :: Maybe Int -> BrowserAction ()
setMaxRedirects c
 | fromMaybe 0 c < 0  = return ()
 | otherwise          = modify (\b -> b{bsMaxRedirects=c})

-- | @getMaxRedirects@ returns the current setting for the max-redirect count.
-- If @Nothing@, the "Network.Browser"'s default is used.
getMaxRedirects :: BrowserAction (Maybe Int)
getMaxRedirects = gets bsMaxRedirects

-- | @setMaxPoolSize maxCount@ sets the maximum size of the connection pool
-- that is used to cache connections between requests
setMaxPoolSize :: Maybe Int -> BrowserAction ()
setMaxPoolSize c = modify (\b -> b{bsMaxPoolSize=c})

-- | @getMaxPoolSize@ gets the maximum size of the connection pool
-- that is used to cache connections between requests.
-- If @Nothing@, the "Network.Browser"'s default is used.
getMaxPoolSize :: BrowserAction (Maybe Int)
getMaxPoolSize = gets bsMaxPoolSize

-- | @setProxy p@ will disable proxy usage if @p@ is @NoProxy@.
-- If @p@ is @Proxy proxyURL mbAuth@, then @proxyURL@ is interpreted
-- as the URL of the proxy to use, possibly authenticating via
-- 'Authority' information in @mbAuth@.
setProxy :: Proxy -> BrowserAction ()
setProxy p =
   -- Note: if user _explicitly_ sets the proxy, we turn
   -- off any auto-detection of proxies.
  modify (\b -> b {bsProxy = p, bsCheckProxy=False})

-- | @getProxy@ returns the current proxy settings. If
-- the auto-proxy flag is set to @True@, @getProxy@ will
-- perform the necessary
getProxy :: BrowserAction Proxy
getProxy = do
  p <- gets bsProxy
  case p of
      -- Note: if there is a proxy, no need to perform any auto-detect.
      -- Presumably this is the user's explicit and preferred proxy server.
    Proxy{} -> return p
    NoProxy{} -> do
     flg <- gets bsCheckProxy
     if not flg
      then return p
      else do
       np <- liftIO $ fetchProxy True{-issue warning on stderr if ill-formed...-}
        -- note: this resets the check-proxy flag; a one-off affair.
       setProxy np
       return np

-- | @setCheckForProxy flg@ sets the one-time check for proxy
-- flag to @flg@. If @True@, the session will try to determine
-- the proxy server is locally configured. See 'Network.HTTP.Proxy.fetchProxy'
-- for details of how this done.
setCheckForProxy :: Bool -> BrowserAction ()
setCheckForProxy flg = modify (\ b -> b{bsCheckProxy=flg})

-- | @getCheckForProxy@ returns the current check-proxy setting.
-- Notice that this may not be equal to @True@ if the session has
-- set it to that via 'setCheckForProxy' and subsequently performed
-- some HTTP protocol interactions. i.e., the flag return represents
-- whether a proxy will be checked for again before any future protocol
-- interactions.
getCheckForProxy :: BrowserAction Bool
getCheckForProxy = gets bsCheckProxy

-- | @setUserAgent ua@ sets the current @User-Agent:@ string to @ua@. It
-- will be used if no explicit user agent header is found in subsequent requests.
--
-- A common form of user agent string is @\"name\/version (details)\"@. For
-- example @\"cabal-install/0.10.2 (HTTP 4000.1.2)\"@. Including the version
-- of this HTTP package can be helpful if you ever need to track down HTTP
-- compatibility quirks. This version is available via 'httpPackageVersion'.
-- For more info see <http://en.wikipedia.org/wiki/User_agent>.
--
setUserAgent :: String -> BrowserAction ()
setUserAgent ua = modify (\b -> b{bsUserAgent=Just ua})

-- | @getUserAgent@ returns the current @User-Agent:@ default string.
getUserAgent :: BrowserAction String
getUserAgent  = do
  n <- gets bsUserAgent
  return (maybe defaultUserAgent id n)

-- | @RequestState@ is an internal tallying type keeping track of various
-- per-connection counters, like the number of authorization attempts and
-- forwards we've gone through.
data RequestState
  = RequestState
      { reqDenies     :: Int   -- ^ number of 401 responses so far
      , reqRedirects  :: Int   -- ^ number of redirects so far
      , reqRetries    :: Int   -- ^ number of retries so far
      , reqStopOnDeny :: Bool  -- ^ whether to pre-empt 401 response
      }

type RequestID = Int -- yeah, it will wrap around.

nullRequestState :: RequestState
nullRequestState = RequestState
      { reqDenies     = 0
      , reqRedirects  = 0
      , reqRetries    = 0
      , reqStopOnDeny = True
      }

-- | @BrowserEvent@ is the event record type that a user-defined handler, set
-- via 'setEventHandler', will be passed. It indicates various state changes
-- encountered in the processing of a given 'RequestID', along with timestamps
-- at which they occurred.
data BrowserEvent
 = BrowserEvent
      { browserTimestamp  :: UTCTime
      , browserRequestID  :: RequestID
      , browserRequestURI :: {-URI-}String
      , browserEventType  :: BrowserEventType
      }

-- | 'BrowserEventType' is the enumerated list of events that the browser
-- internals will report to a user-defined event handler.
data BrowserEventType
 = OpenConnection
 | ReuseConnection
 | RequestSent
 | ResponseEnd ResponseData
 | ResponseFinish
{- not yet, you will have to determine these via the ResponseEnd event.
 | Redirect
 | AuthChallenge
 | AuthResponse
-}

-- | @setEventHandler onBrowserEvent@ configures event handling.
-- If @onBrowserEvent@ is @Nothing@, event handling is turned off;
-- setting it to @Just onEv@ causes the @onEv@ IO action to be
-- notified of browser events during the processing of a request
-- by the Browser pipeline.
setEventHandler :: Maybe (BrowserEvent -> BrowserAction ()) -> BrowserAction ()
setEventHandler mbH = modify (\b -> b { bsEvent=mbH})

buildBrowserEvent :: BrowserEventType -> {-URI-}String -> RequestID -> IO BrowserEvent
buildBrowserEvent bt uri reqID = do
  ct <- getCurrentTime
  return BrowserEvent
         { browserTimestamp  = ct
         , browserRequestID  = reqID
         , browserRequestURI = uri
         , browserEventType  = bt
         }

reportEvent :: BrowserEventType -> {-URI-}String -> BrowserAction ()
reportEvent bt uri = do
  st <- get
  case bsEvent st of
    Nothing -> return ()
    Just evH -> do
       evt <- liftIO $ buildBrowserEvent bt uri (bsRequestID st)
       evH evt -- if it fails, we fail.

-- | The default number of hops we are willing not to go beyond for
-- request forwardings.
defaultMaxRetries :: Int
defaultMaxRetries = 4

-- | The default number of error retries we are willing to perform.
defaultMaxErrorRetries :: Int
defaultMaxErrorRetries = 4

-- | The default maximum HTTP Authentication attempts we will make for
-- a single request.
defaultMaxAuthAttempts :: Int
defaultMaxAuthAttempts = 2

-- | The default setting for auto-proxy detection.
-- You may change this within a session via 'setAutoProxyDetect'.
-- To avoid initial backwards compatibility issues, leave this as @False@.
defaultAutoProxyDetect :: Bool
defaultAutoProxyDetect = False

-- | @request httpRequest@ tries to submit the 'Request' @httpRequest@
-- to some HTTP server (possibly going via a /proxy/, see 'setProxy'.)
-- Upon successful delivery, the URL where the response was fetched from
-- is returned along with the 'Response' itself.
request :: Request
        -> BrowserAction (URI,Response)
request req = nextRequest $
  catchBrowseException
    (do res <- request' [] initialState req
        reportEvent ResponseFinish (show (rqURI req))
        return res)
    (\(e :: SomeException) -> do
        let errStr = "Network.Browser.request: Error raised " ++ show e
        err errStr
        Prelude.fail errStr)
 where
  initialState = nullRequestState

-- | Internal helper function, explicitly carrying along per-request
-- counts.
request' :: String
         -> RequestState
         -> Request
         -> BrowserAction (URI,Response)
request' nullVal rqState rq = do
   let uri = rqURI rq
   uria <- getAuth rq
     -- add cookies to request
   cookies <- getCookiesFor (uriAuthToString id (Just uria) "") (uriPath uri)
   when (not $ null cookies)
        (out $ "Adding cookies to request.  Cookie names: "  ++ unwords (map ckName cookies))
    -- add credentials to request
   rq' <-
    if not (reqStopOnDeny rqState)
     then return rq
     else do
       auth <- anticipateChallenge rq
       case auth of
         Nothing -> return rq
         Just x  -> return (insertHeader HdrAuthorization (withAuthority x rq) rq)
   let rq'' = if not $ null cookies then insertHeaders [Header HdrCookie (renderCookies cookies)] rq' else rq'
   p <- getProxy
   def_ua <- gets bsUserAgent
   let defaultOpts =
         case p of
           NoProxy     -> defaultNormalizeRequestOptions{normUserAgent=def_ua}
           Proxy _ ath ->
              defaultNormalizeRequestOptions
                { normForProxy  = True
                , normUserAgent = def_ua
                , normCustoms   =
                    maybe []
                          (\authS -> [\ _ r -> insertHeader HdrProxyAuthorization (withAuthority authS r) r])
                          ath
                }
   let final_req = normalizeRequest defaultOpts rq''
   out ("Sending:\n" ++ show final_req)
   catchBrowseException
      (do rsp <- case p of
                   NoProxy        -> do auth <- getAuth rq''
                                        dorequest auth final_req
                   Proxy str _ath -> do
                      let notURI
                            | null pt || null hst =
                                URIAuth{ uriUserInfo = ""
                                       , uriRegName  = str
                                       , uriPort     = ""
                                       }
                            | otherwise =
                                URIAuth{ uriUserInfo = ""
                                       , uriRegName  = hst
                                       , uriPort     = pt
                                       }
                            -- If the ':' is dropped from port below, dorequest will assume port 80. Leave it!
                            where (hst, pt) = span (':'/=) str
                      -- Proxy can take multiple forms - look for http://host:port first,
                      -- then host:port. Fall back to just the string given (probably a host name).
                      let proxyURIAuth =
                            maybe notURI
                                  (\parsed -> maybe notURI id (uriAuthority parsed))
                                  (parseURI str)

                      out $ "proxy uri host: " ++ uriRegName proxyURIAuth ++ ", port: " ++ uriPort proxyURIAuth
                      dorequest proxyURIAuth final_req
              
          out ("Received:\n" ++ show rsp)
          -- add new cookies to browser state
          auth <- getAuth rq
          handleCookies uri (uriAuthToString id (Just auth) "")
                            (retrieveHeaders HdrSetCookie rsp)
          -- Deal with "Connection: close" in response.
          handleConnectionClose auth (retrieveHeaders HdrConnection rsp)
          mbMxAuths <- getMaxAuthAttempts
          case rspCode rsp of
           (4,0,1) -- Credentials not sent or refused.
             | reqDenies rqState > fromMaybe defaultMaxAuthAttempts mbMxAuths -> do
                 out "401 - credentials again refused; exceeded retry count (2)"
                 return (uri,rsp)
             | otherwise -> do
                 out "401 - credentials not supplied or refused; retrying.."
                 let hdrs = retrieveHeaders HdrWWWAuthenticate rsp
                 flg <- getAllowBasicAuth
                 case pickChallenge flg (catMaybes $ map (headerToChallenge uri) hdrs) of
                   Nothing -> do
                     out "no challenge"
                     return (uri,rsp)   {- do nothing -}
                   Just x  -> do
                     au <- challengeToAuthority uri x
                     case au of
                       Nothing  -> do
                         out "no auth"
                         return (uri,rsp) {- do nothing -}
                       Just au' -> do
                         out "Retrying request with new credentials"
                         request' nullVal
                                  rqState{ reqDenies     = succ(reqDenies rqState)
                                         , reqStopOnDeny = False
                                         }
                                  (insertHeader HdrAuthorization (withAuthority au' rq) rq)

           (4,0,7)  -- Proxy Authentication required
             | reqDenies rqState > fromMaybe defaultMaxAuthAttempts mbMxAuths -> do
                 out "407 - proxy authentication required; max deny count exceeeded (2)"
                 return (uri,rsp)
             | otherwise -> do
                 out "407 - proxy authentication required"
                 let hdrs = retrieveHeaders HdrProxyAuthenticate rsp
                 flg <- getAllowBasicAuth
                 case pickChallenge flg (catMaybes $ map (headerToChallenge uri) hdrs) of
                   Nothing -> return (uri,rsp)   {- do nothing -}
                   Just x  -> do
                     au <- challengeToAuthority uri x
                     case au of
                       Nothing  -> return (uri,rsp)  {- do nothing -}
                       Just au' -> do
                         pxy <- gets bsProxy
                         case pxy of
                           NoProxy -> do
                             err "Proxy authentication required without proxy!"
                             return (uri,rsp)
                           Proxy px _ -> do
                             out "Retrying with proxy authentication"
                             setProxy (Proxy px (Just au'))
                             request' nullVal
                                      rqState{ reqDenies     = succ(reqDenies rqState)
                                             , reqStopOnDeny = False
                                             }
                                      rq

           (3,0,x) | x `elem` [2,3,1,7]  ->  do
             out ("30" ++ show x ++  " - redirect")
             allow_redirs <- allowRedirect rqState
             case allow_redirs of
               False -> return (uri,rsp)
               _     -> do
                  case retrieveHeaders HdrLocation rsp of
                    [] -> do err "No Location: header in redirect response"
                             return (uri,rsp)
                    (Header _ u:_) ->
                        case parseURIReference u of
                          Nothing -> do err ("Parse of Location: header in a redirect response failed: " ++ u)
                                        return (uri,rsp)
                          Just newURI
                              | {-uriScheme newURI_abs /= uriScheme uri && -}(not (supportedScheme newURI_abs)) -> do
                                   err ("Unable to handle redirect, unsupported scheme: " ++ show newURI_abs)
                                   return (uri, rsp)
                              | otherwise -> do
                                   out ("Redirecting to " ++ show newURI_abs ++ " ...")

                                   -- Redirect using GET request method, depending on
                                   -- response code.
                                   let toGet = x `elem` [2,3]
                                       method = if toGet then GET else rqMethod rq
                                       rq1 = rq { rqMethod=method, rqURI=newURI_abs }
                                       rq2 = if toGet then replaceHeader HdrContentLength "0" (rq1 {rqBody = nullVal}) else rq1

                                   request' nullVal
                                            rqState{ reqDenies     = 0
                                                   , reqRedirects  = succ(reqRedirects rqState)
                                                   , reqStopOnDeny = True
                                                   }
                                            rq2
                              where
                                newURI_abs = uriDefaultTo newURI uri

           (3,0,5) ->
             case retrieveHeaders HdrLocation rsp of
               [] -> do err "No Location header in proxy redirect response."
                        return (uri,rsp)
               (Header _ u:_) ->
                  case parseURIReference u of
                    Nothing -> do err ("Parse of Location header in a proxy redirect response failed: " ++ u)
                                  return (uri,rsp)
                    Just newuri -> do
                       out ("Retrying with proxy " ++ show newuri ++ "...")
                       setProxy (Proxy (uriAuthToString id (uriAuthority newuri) "") Nothing)
                       request' nullVal
                                rqState{ reqDenies     = 0
                                       , reqRedirects  = 0
                                       , reqRetries    = succ (reqRetries rqState)
                                       , reqStopOnDeny = True
                                       }
                                rq
           _       -> return (uri,rsp))
        (\e -> do
           mbMx <- getMaxErrorRetries
           if (reqRetries rqState < fromMaybe defaultMaxErrorRetries mbMx) &&
              (e == ErrorReset || e == ErrorClosed)
             then do --empty connnection pool in case connection has become invalid
                     modify (\b -> b { bsConnectionPool=[] })
                     request' nullVal rqState{reqRetries=succ (reqRetries rqState)} rq
             else liftIO (throwIO e))

-- | The internal request handling state machine.
dorequest :: URIAuth
          -> Request
          -> BrowserAction Response
dorequest auth rqst = do
  pool <- gets bsConnectionPool
  conn <- liftIO $ filterM (\c -> isTCPConnectedTo c (uriRegName auth) (uriAuthPort Nothing auth)) pool
  rsp <-
    case conn of
      [] -> do
        out ("Creating new connection to " ++ uriAuthToString id (Just auth) "")
        reportEvent OpenConnection (show (rqURI rqst))
        c <- liftIO $ openTCPConnection Nothing (uriRegName auth) (uriAuthPort Nothing auth)
        updateConnectionPool c
        dorequest2 c rqst
      (c:_) -> do
        out ("Recovering connection to " ++ uriAuthToString id (Just auth) "")
        reportEvent ReuseConnection (show (rqURI rqst))
        dorequest2 c rqst
  case rsp of
    Response a b c _ ->
      reportEvent (ResponseEnd (a,b,c)) (show (rqURI rqst)) ; _ -> return ()
  return rsp
 where
  dorequest2 c r = do
    st  <- get
    let
     onSendComplete =
       maybe (return ())
             (\evh -> do
                x <- buildBrowserEvent RequestSent (show (rqURI r)) (bsRequestID st)
                runBA st (evh x)
                return ())
             (bsEvent st)
    liftIO $ sendHTTP_notify c r onSendComplete

updateConnectionPool :: Connection -> BrowserAction ()
updateConnectionPool c = do
   pool <- gets bsConnectionPool
   let len_pool = length pool
   maxPoolSize <- fromMaybe defaultMaxPoolSize <$> gets bsMaxPoolSize
   when (len_pool > maxPoolSize)
        (liftIO $ close (last pool))
   let pool'
        | len_pool > maxPoolSize = init pool
        | otherwise              = pool
   when (maxPoolSize > 0) $ modify (\b -> b { bsConnectionPool=c:pool' })
   return ()

-- | Default maximum number of open connections we are willing to have active.
defaultMaxPoolSize :: Int
defaultMaxPoolSize = 5

cleanConnectionPool :: URIAuth -> BrowserAction ()
cleanConnectionPool auth = do
  pool <- gets bsConnectionPool
  bad <- liftIO $ mapM (\c -> isTCPConnectedTo c (uriRegName auth) (uriAuthPort Nothing auth)) pool
  let tmp = zip bad pool
      newpool = map snd $ filter (not . fst) tmp
      toclose = map snd $ filter fst tmp
  liftIO $ forM_ toclose close
  modify (\b -> b { bsConnectionPool = newpool })

handleCookies :: URI -> String -> [Header] -> BrowserAction ()
handleCookies _   _              [] = return () -- cut short the silliness.
handleCookies uri dom cookieHeaders = do
  when (not $ null errs)
       (err $ unlines ("Errors parsing these cookie values: ":errs))
  when (not $ null newCookies)
       (out $ foldl (\x y -> x ++ "\n  " ++ show y) "Cookies received:" newCookies)
  filterfn    <- getCookieFilter
  newCookies' <- liftIO (filterM (filterfn uri) newCookies)
  when (not $ null newCookies')
       (out $ "Accepting cookies with names: " ++ unwords (map ckName newCookies'))
  mapM_ addCookie newCookies'
 where
  (errs, newCookies) = headersToCookies dom HdrSetCookie cookieHeaders

handleConnectionClose :: URIAuth -> [Header] -> BrowserAction ()
handleConnectionClose _ [] = return ()
handleConnectionClose uri headers = do
  let doClose = any (== "close") $ map headerToConnType headers
  when doClose $ cleanConnectionPool uri
  where headerToConnType (Header _ t) = map toLower t

------------------------------------------------------------------
----------------------- Miscellaneous ----------------------------
------------------------------------------------------------------

allowRedirect :: RequestState -> BrowserAction Bool
allowRedirect rqState = do
  rd <- getAllowRedirects
  mbMxRetries <- getMaxRedirects
  return (rd && (reqRedirects rqState <= fromMaybe defaultMaxRetries mbMxRetries))

-- | Return @True@ iff the package is able to handle requests and responses
-- over it.
supportedScheme :: URI -> Bool
supportedScheme u = uriScheme u == "http:"

-- | @uriDefaultTo a b@ returns a URI that is consistent with the first
-- argument URI @a@ when read in the context of the second URI @b@.
-- If the second argument is not sufficient context for determining
-- a full URI then anarchy reins.
uriDefaultTo :: URI -> URI -> URI
#if MIN_VERSION_network(2,4,0)
uriDefaultTo a b = a `relativeTo` b
#else
uriDefaultTo a b = maybe a id (a `relativeTo` b)
#endif


-- This form junk is completely untested...

type FormVar = (String,String)

data Form = Form RequestMethod URI [FormVar]

formToRequest :: Form -> Request
formToRequest (Form m u vs) =
    let enc = urlEncodeVars vs
    in case m of
        GET -> Request { rqMethod=GET
                       , rqHeaders=[ Header HdrContentLength "0" ]
                       , rqBody=""
                       , rqURI=u { uriQuery= '?' : enc }  -- What about old query?
                       }
        POST -> Request { rqMethod=POST
                        , rqHeaders=[ Header HdrContentType "application/x-www-form-urlencoded",
                                      Header HdrContentLength (show $ length enc) ]
                        , rqBody=enc
                        , rqURI=u
                        }
        _ -> error ("unexpected request: " ++ show m)


-- Encode form variables, useable in either the
-- query part of a URI, or the body of a POST request.
-- I have no source for this information except experience,
-- this sort of encoding worked fine in CGI programming.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
             urlEncode = escapeURIString isUnescapedInURI
urlEncodeVars [] = []
