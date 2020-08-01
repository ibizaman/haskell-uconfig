{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Config.SystemdService
Description : Systemd service file parser
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Config.SystemdService module supports parsing and generating Systemd files.

For now, it only supports [Service files](https://www.freedesktop.org/software/systemd/man/systemd.service.html).

The main type this module exposes is SystemdService.
-}
module Config.SystemdService
    (
    -- |Main type
      SystemdService(..)

    -- |Base types
    , SBool(svalue, stype)
    , SBoolType(..)
    , sFalse
    , sTrue
    , setValue
    , setType
    , One
    , none
    , one
    , disableAll
    , appendOne
    , getAllFromOne
    , Words(..)
    , List(..)
    , ResettableList(..)
    , ListElem(..)
    , computeResettableList

    -- |Systemd specific types
    , Unit(..)
    , Description(..)
    , Documentation(..)
    , Target(..)
    , Install(..)
    , Service(..)
    , Type(..)
    , Exec(..)
    , User(..)
    , Group(..)
    , PIDFile(..)
    , RemainAfterExit(..)
    , BusName(..)
    , NotifyAccess(..)
    , WorkingDirectory(..)
    , Output(..)
    , PrivateTmp(..)
    , Restart(..)
    , KillMode(..)
    , ProtectHome(..)
    , ProtectSystem(..)
    , Capability(..)
    , InclusionCapabilities(..)
    , CapabilitiesSet
    , Limit(..)
    , Infinity(..)
    , Time(..)
    , TimeUnit(..)
    )
where


import           Data.Functor                   ( ($>) )
import qualified Data.String
import qualified Data.Text                     as T
import           Data.Semigroup                 ( Semigroup(..)
                                                , Last(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Generic.Data                   ( Generically(..) )
import qualified Parser                        as P
import qualified Config                        as C
import qualified Syntax                        as S
import           Syntax                         ( (/*?)
                                                , (/**)
                                                , (/**?)
                                                )


-- |Value can be either present or not. There can be at most one
-- enabled value but there can be any number of disabled one.
newtype One a = One [S.Value a]
    deriving (Eq, Show, Generic, Functor)
    deriving (Semigroup, Monoid) via (Generically (One a))

none :: One a
none = One []

one :: S.Value a -> One a
one v = One [v]

disableAll :: One a -> One a
disableAll (One vs) = One $ S.setEnabled False <$> vs

appendOne :: S.Value a -> One a -> One a
appendOne v vs =
    let (One vs') = if S.enabled v then disableAll vs else vs
    in  One $ vs' <> [v]

getAllFromOne :: One a -> [S.Value a]
getAllFromOne (One vs) = vs

instance C.Config T.Text v => C.Config [S.Value T.Text] (One v) where
    parser vs =
        One <$> (C.mergeParseResult $ fmap (C.liftValue . fmap C.parser) vs)

    unparser (One vs) = fmap (fmap C.unparser) vs


newtype Words a = Words [a]
    deriving (Eq, Show, Generic, Functor)
    deriving (Semigroup, Monoid) via (Generically (Words a))

instance C.Config T.Text v => C.Config T.Text (Words v) where
    parser vs = Words <$> (C.mergeParseResult $ fmap C.parser $ T.words vs)

    unparser (Words vs) = T.unwords $ fmap C.unparser vs


newtype List a = List [S.Value a]
    deriving (Eq, Show, Generic, Functor)
    deriving (Semigroup, Monoid) via (Generically (List a))

instance C.Config T.Text v => C.Config [S.Value T.Text] (List v) where
    parser ls = List <$> C.mergeParseResult (fmap convert ls)
      where
        convert
            :: C.Config T.Text v => S.Value T.Text -> C.ParseResult (S.Value v)
        convert v = case C.parser $ S.value v of
            C.ParseError   e   -> C.ParseError e
            C.ParseSuccess vs' -> C.ParseSuccess (v { S.value = vs' })

    unparser (List ls) = fmap (\v -> v { S.value = C.unparser $ S.value v }) ls

newtype ResettableList a = ResettableList [S.Value (ListElem a)]
    deriving (Eq, Show, Generic, Functor)
    deriving (Semigroup, Monoid) via (Generically (ResettableList a))

computeResettableList :: [ListElem a] -> [a]
computeResettableList = foldr f []
  where
    f ListReset    _  = []
    f (ListElem v) vs = v : vs

data ListElem a = ListReset | ListElem a
    deriving(Eq, Show, Functor)

instance C.Config T.Text v => C.Config T.Text (ListElem v) where
    parser "" = C.ParseSuccess ListReset
    parser t  = ListElem <$> (C.parser t)

    unparser ListReset    = ""
    unparser (ListElem t) = C.unparser t

instance C.Config T.Text v => C.Config [S.Value T.Text] (ResettableList v) where
    parser ls = ResettableList <$> C.mergeParseResult (fmap convert ls)
      where
        convert
            :: C.Config T.Text v
            => S.Value T.Text
            -> C.ParseResult (S.Value (ListElem v))
        convert v = case S.value v of
            "" -> C.ParseSuccess $ v { S.value = ListReset }
            v' -> case C.parser v' of
                C.ParseError   e   -> C.ParseError e
                C.ParseSuccess v'' -> C.ParseSuccess (v { S.value = v'' })

    unparser (ResettableList ls) =
        fmap (\v -> v { S.value = C.unparser $ S.value v }) ls


-- |Type of a Systemd boolean value
data SBoolType
    = BYes  -- ^boolean is either 'yes' or 'no'.
    | BNum  -- ^boolean is either '1' or '0'.
    | BTrue -- ^boolean is either 'true' or 'false'.
    | BOn   -- ^boolean is either 'on' or 'off'.
    deriving(Show, Eq)

-- |A boolean that also knows in what style it was written.
data SBool = SBool
    { svalue :: Bool
    , stype :: SBoolType
    }
    deriving(Show, Eq)

sFalse :: SBool
sFalse = SBool False BTrue

sTrue :: SBool
sTrue = SBool True BTrue

instance Semigroup SBool where
    a <> b = b { svalue = svalue a || svalue b }

instance Monoid SBool where
    mempty = SBool False BTrue

setType :: SBoolType -> SBool -> SBool
setType t b = b { stype = t }

setValue :: Bool -> SBool -> SBool
setValue v b = b { svalue = v }

instance C.Config T.Text SBool where
    parser = C.parseText sboolParser

    unparser v = case (stype v, svalue v) of
        (BTrue, True ) -> "true"
        (BTrue, False) -> "false"
        (BYes , True ) -> "yes"
        (BYes , False) -> "no"
        (BOn  , True ) -> "on"
        (BOn  , False) -> "off"
        (BNum , True ) -> "1"
        (BNum , False) -> "0"

sboolParser :: (Ord e) => P.Parser e SBool
sboolParser = P.choice
    [ "true" $> SBool True BTrue
    , "yes" $> SBool True BYes
    , "on" $> SBool True BOn
    , "1" $> SBool True BNum
    , "false" $> SBool False BTrue
    , "no" $> SBool False BYes
    , "off" $> SBool False BOn
    , "0" $> SBool False BNum
    ]


-- |A Systemd Service record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html
data SystemdService = SystemdService
  { unit :: Unit
  , install :: Install
  , service :: Service
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically SystemdService)

instance C.Config S.XDGDesktop SystemdService where
    parser xdg =
        SystemdService
            <$> C.parser (S.getSection xdg "Unit")
            <*> C.parser (S.getSection xdg "Install")
            <*> C.parser (S.getSection xdg "Service")

    unparser s =
        mempty
            /*? (Just "Unit"   , C.unparser (unit s))
            /*? (Just "Install", C.unparser (install s))
            /*? (Just "Service", C.unparser (service s))

-- |A Systemd [Unit] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BUnit%5D%20Section%20Options
data Unit = Unit
  { description :: One Description
  , documentation :: List (Words Documentation)
  , wants :: ResettableList (Words Target)
  , requires :: ResettableList (Words Target)
  , requisite :: ResettableList (Words Target)
  , bindsTo :: ResettableList (Words Target)
  , partOf :: ResettableList (Words Target)
  , conflicts :: ResettableList (Words Target)
  , before :: ResettableList (Words Target)
  , after :: ResettableList (Words Target)
  , onFailure :: ResettableList (Words Target)
  , propagatesReloadTo :: ResettableList (Words Target)
  , reloadPropagatedFrom :: ResettableList (Words Target)
  , joinsNamespaceOf :: ResettableList (Words Target)
  , requiresMountsFor :: List T.Text
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically Unit)

instance C.Config S.Section Unit where
    parser sec =
        Unit
            <$> (C.parser (S.getValue sec "Description"))
            <*> (C.parser (S.getValue sec "Documentation"))
            <*> (C.parser (S.getValue sec "Wants"))
            <*> (C.parser (S.getValue sec "Requires"))
            <*> (C.parser (S.getValue sec "Requisite"))
            <*> (C.parser (S.getValue sec "BindsTo"))
            <*> (C.parser (S.getValue sec "PartOf"))
            <*> (C.parser (S.getValue sec "Conflicts"))
            <*> (C.parser (S.getValue sec "Before"))
            <*> (C.parser (S.getValue sec "After"))
            <*> (C.parser (S.getValue sec "OnFailure"))
            <*> (C.parser (S.getValue sec "PropagatesReloadTo"))
            <*> (C.parser (S.getValue sec "ReloadPropagatedFrom"))
            <*> (C.parser (S.getValue sec "JoinsNamespaceOf"))
            <*> (C.parser (S.getValue sec "RequiresMountsFor"))

    unparser u =
        mempty
            /**? ("Description"       , C.unparser $ description u)
            /**? ("Documentation"     , C.unparser $ documentation u)
            /**? ("Wants"             , C.unparser $ wants u)
            /**? ("Requires"          , C.unparser $ requires u)
            /**? ("Requisite"         , C.unparser $ requisite u)
            /**? ("BindsTo"           , C.unparser $ bindsTo u)
            /**? ("PartOf"            , C.unparser $ partOf u)
            /**? ("Conflicts"         , C.unparser $ conflicts u)
            /**? ("Before"            , C.unparser $ before u)
            /**? ("After"             , C.unparser $ after u)
            /**? ("OnFailure"         , C.unparser $ onFailure u)
            /**? ("PropagatesReloadTo", C.unparser $ propagatesReloadTo u)
            /**? ("JoinsNamespaceOf"  , C.unparser $ joinsNamespaceOf u)
            /**? ("RequiresMountsFor" , C.unparser $ requiresMountsFor u)

newtype Description = Description T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Description)

instance Data.String.IsString Description where
    fromString = Description . T.pack

instance C.Config T.Text Description where
    parser = fmap Description . C.parseText P.words

    unparser (Description d) = d


data Documentation
    = DocHTTP T.Text
    | DocHTTPS T.Text
    | DocFile T.Text
    | DocInfo T.Text
    | DocMan T.Text
    deriving (Eq, Show, Generic)

instance Data.String.IsString Documentation where
    fromString s = case C.parser $ T.pack s of
        C.ParseSuccess d -> d
        C.ParseError   _ -> DocFile $ T.pack s

instance C.Config T.Text Documentation where
    parser = C.parseText
        (C.plain <$> C.spaced
            (C.quoted
                (P.choice
                    [ P.chunk "http://" >> DocHTTP <$> P.word
                    , P.chunk "https://" >> DocHTTPS <$> P.word
                    , P.chunk "file:" >> DocFile <$> P.word
                    , P.chunk "info:" >> DocInfo <$> P.word
                    , P.chunk "man:" >> DocMan <$> P.word
                    ]
                )
            )
        )

    unparser (DocHTTP  d) = "http://" <> d
    unparser (DocHTTPS d) = "https://" <> d
    unparser (DocFile  d) = "file:" <> d
    unparser (DocInfo  d) = "info:" <> d
    unparser (DocMan   d) = "man:" <> d


newtype Target = Target T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Target)

instance Data.String.IsString Target where
    fromString = Target . T.pack

instance C.Config T.Text Target where
    parser = fmap Target . C.parseText (C.plain <$> C.spaced (C.quoted P.word))

    unparser (Target t) = t

instance C.Config T.Text [Target] where
    parser   = sequenceA . fmap C.parser . T.words

    unparser = T.unwords . fmap C.unparser


-- |A Systemd [Install] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BInstall%5D%20Section%20Options
data Install = Install
    { alias :: List (Words T.Text)
    , wantedBy :: ResettableList (Words Target)
    , requiredBy :: ResettableList (Words Target)
    , also :: List (Words Target)
    , defaultInstance :: One T.Text
    }
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Install)

instance C.Config S.Section Install where
    parser sec =
        Install
            <$> (C.parser (S.getValue sec "Alias"))
            <*> (C.parser (S.getValue sec "WantedBy"))
            <*> (C.parser (S.getValue sec "RequiredBy"))
            <*> (C.parser (S.getValue sec "Also"))
            <*> (C.parser (S.getValue sec "DefaultInstance"))

    unparser u =
        mempty
            /**? ("Alias"          , C.unparser $ alias u)
            /**? ("WantedBy"       , C.unparser $ wantedBy u)
            /**? ("RequiredBy"     , C.unparser $ requiredBy u)
            /**? ("Also"           , C.unparser $ also u)
            /**? ("DefaultInstance", C.unparser $ defaultInstance u)


-- |A Systemd [Service] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Options
-- https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#TasksMax=N
data Service = Service
    { execCondition :: List Exec
    , execStartPre :: List Exec
    , execStartPost :: List Exec
    , type_ :: Type
    , execReload :: List Exec
    , execStop :: List Exec
    , execStopPost :: List Exec
    , remainAfterExit :: One RemainAfterExit
    , user :: One User
    , group :: One Group
    , workingDirectory :: One WorkingDirectory
    , standardOutput :: One Output
    , standardError :: One Output
    , tasksMax :: One (Infinity Int)
    , restart :: One Restart
    , privateTmp :: One PrivateTmp
    , killMode :: One KillMode
    , protectHome :: One ProtectHome
    , protectSystem :: One ProtectSystem
    , capabilityBoundingSet :: CapabilitiesSet
    , timeoutStartSec :: One (Infinity Time)
    , delegate :: One SBool
    , limitCPU :: One (Limit Time)
    , limitFSIZE :: One (Limit Byte)
    , limitDATA :: One (Limit Byte)
    , limitSTACK :: One (Limit Byte)
    , limitCORE :: One (Limit Byte)
    , limitRSS :: One (Limit Byte)
    , limitNOFILE :: One (Limit Int)
    , limitAS :: One (Limit Byte)
    , limitNPROC :: One (Limit Int)
    , limitMEMLOCK :: One (Limit Byte)
    , limitLOCKS :: One (Limit Int)
    , limitSIGPENDING :: One (Limit Int)
    , limitMSGQUEUE :: One (Limit Byte)
    , limitNICE :: One (Limit Int)
    , limitRTPRIO :: One (Limit Int)
    , limitRTTIME :: One (Limit Time)
    , startLimitBurst :: One Int
    , startLimitInterval :: One Time
    }
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Service)

data Limit a
    = LimitBoth (Infinity a)
    | LimitSeparate (Infinity a) (Infinity a)
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last (Limit a)))

instance C.Config T.Text a => C.Config T.Text (Limit a) where
    parser t = f t >>= \case
        (first, Nothing) -> LimitBoth <$> C.parser first
        (first, Just second) ->
            LimitSeparate <$> C.parser first <*> C.parser second
      where
        f = C.parseText $ do
            first  <- P.word
            isBoth <- P.optional ":"
            case isBoth of
                Nothing -> return (first, Nothing)
                Just _  -> do
                    second <- P.word
                    return (first, Just second)

    unparser (LimitBoth a      ) = C.unparser a
    unparser (LimitSeparate a b) = C.unparser a <> ":" <> C.unparser b

data Infinity a = Infinity | NotInfinity a
    deriving (Eq, Show, Generic, Functor)
    deriving (Semigroup, Monoid) via (Generically (Last (Infinity a)))

instance C.Config T.Text a => C.Config T.Text (Infinity a) where
    parser t =
        C.parseText
                (P.choice [P.try "infinity" $> Infinity, NotInfinity <$> P.word]
                )
                t
            >>= \p -> liftParseResult $ fmap C.parser p

      where
        liftParseResult
            :: Infinity (C.ParseResult a) -> C.ParseResult (Infinity a)
        liftParseResult Infinity                       = C.ParseSuccess Infinity
        liftParseResult (NotInfinity (C.ParseError e)) = C.ParseError e
        liftParseResult (NotInfinity (C.ParseSuccess v)) =
            C.ParseSuccess (NotInfinity v)

    unparser (NotInfinity v) = C.unparser v
    unparser Infinity        = "infinity"

data Byte = K Int | M Int | G Int | T Int | P Int | E Int | ByteNoUnit Int
    deriving (Eq, Show)

instance C.Config T.Text Byte where
    parser = C.parseText $ do
        n <- P.number
        u <- P.choice
            [ P.try "K" $> K
            , P.try "M" $> M
            , P.try "G" $> G
            , P.try "T" $> T
            , P.try "P" $> P
            , P.try "E" $> E
            , "" $> ByteNoUnit
            ]
        return $ u n

    unparser (K          n) = T.pack (show n) <> "K"
    unparser (M          n) = T.pack (show n) <> "M"
    unparser (G          n) = T.pack (show n) <> "G"
    unparser (T          n) = T.pack (show n) <> "T"
    unparser (P          n) = T.pack (show n) <> "P"
    unparser (E          n) = T.pack (show n) <> "E"
    unparser (ByteNoUnit n) = T.pack (show n)


data Time = Time [TimeUnit]
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Time)

instance C.Config T.Text Time where
    parser t = Time <$> (p t >>= p')

      where
        p :: T.Text -> C.ParseResult [T.Text]
        p t' =
            P.toList
                <$> C.parseText
                        (P.wordsSepBy P.word (P.space >> return mempty))
                        t'

        p' :: [T.Text] -> C.ParseResult [TimeUnit]
        p' ts = C.mergeParseResult $ fmap C.parser ts

    unparser (Time ts) = mconcat $ fmap C.unparser ts


data TimeUnit
    = Microseconds T.Text Int
    | Milliseconds T.Text Int
    | Seconds T.Text Int
    | Minutes T.Text Int
    | Hours T.Text Int
    | Days T.Text Int
    | Weeks T.Text Int
    | Months T.Text Int
    | Years T.Text Int
    | TimeNoUnit Int
    deriving (Eq, Show)

instance C.Config T.Text TimeUnit where
    parser = C.parseText $ do
        n <- P.number
        u <- P.choice
            [ P.try (P.choice ["usec", "us", "µs"]) >>= return . Microseconds
            , P.try (P.choice ["msec", "ms"]) >>= return . Milliseconds
            , P.try (P.choice ["seconds", "second", "sec", "s"])
            >>= return
            .   Seconds
            , P.try (P.choice ["minutes", "minute", "min", "m"])
            >>= return
            .   Minutes
            , P.try (P.choice ["hours", "hour", "hr", "h"]) >>= return . Hours
            , P.try (P.choice ["days", "day", "d"]) >>= return . Days
            , P.try (P.choice ["weeks", "week", "w"]) >>= return . Weeks
            , P.try (P.choice ["months", "month", "m"]) >>= return . Months
            , P.try (P.choice ["years", "year", "y"]) >>= return . Years
            , "" $> TimeNoUnit
            ]
        return $ u n

    unparser (Microseconds u n) = T.pack (show n) <> u
    unparser (Milliseconds u n) = T.pack (show n) <> u
    unparser (Seconds      u n) = T.pack (show n) <> u
    unparser (Minutes      u n) = T.pack (show n) <> u
    unparser (Hours        u n) = T.pack (show n) <> u
    unparser (Days         u n) = T.pack (show n) <> u
    unparser (Weeks        u n) = T.pack (show n) <> u
    unparser (Months       u n) = T.pack (show n) <> u
    unparser (Years        u n) = T.pack (show n) <> u
    unparser (TimeNoUnit n    ) = T.pack (show n)


instance C.Config S.Section Service where
    parser sec =
        Service
            <$> (C.parser (S.getValue sec "ExecCondition"))
            <*> (C.parser (S.getValue sec "ExecStartPre"))
            <*> (C.parser (S.getValue sec "ExecstartPost"))
            <*> parseType sec
            <*> (C.parser (S.getValue sec "ExecReload"))
            <*> (C.parser (S.getValue sec "ExecStop"))
            <*> (C.parser (S.getValue sec "ExecStopPost"))
            <*> (C.parser (S.getValue sec "RemainAfterExit"))
            <*> (C.parser (S.getValue sec "User"))
            <*> (C.parser (S.getValue sec "Group"))
            <*> (C.parser (S.getValue sec "WorkingDirectory"))
            <*> (C.parser (S.getValue sec "StandardOutput"))
            <*> (C.parser (S.getValue sec "StandardError"))
            <*> (C.parser (S.getValue sec "TasksMax"))
            <*> (C.parser (S.getValue sec "Restart"))
            <*> (C.parser (S.getValue sec "PrivateTmp"))
            <*> (C.parser (S.getValue sec "KillMode"))
            <*> (C.parser (S.getValue sec "ProtectHome"))
            <*> (C.parser (S.getValue sec "ProtectSystem"))
            <*> (C.parser (S.getValue sec "CapabilityBoundingSet"))
            <*> (C.parser (S.getValue sec "TimeoutStartSec"))
            <*> (C.parser (S.getValue sec "Delegate"))
            <*> (C.parser (S.getValue sec "LimitCPU"))
            <*> (C.parser (S.getValue sec "LimitFSIZE"))
            <*> (C.parser (S.getValue sec "LimitDATA"))
            <*> (C.parser (S.getValue sec "LimitSTACK"))
            <*> (C.parser (S.getValue sec "LimitCORE"))
            <*> (C.parser (S.getValue sec "LimitRSS"))
            <*> (C.parser (S.getValue sec "LimitNOFILE"))
            <*> (C.parser (S.getValue sec "LimitAS"))
            <*> (C.parser (S.getValue sec "LimitNPROC"))
            <*> (C.parser (S.getValue sec "LimitMEMLOCK"))
            <*> (C.parser (S.getValue sec "LimitLOCKS"))
            <*> (C.parser (S.getValue sec "LimitSIGPENDING"))
            <*> (C.parser (S.getValue sec "LimitMSGQUEUE"))
            <*> (C.parser (S.getValue sec "LimitNICE"))
            <*> (C.parser (S.getValue sec "LimitRTPRIO"))
            <*> (C.parser (S.getValue sec "LimitRTTIME"))
            <*> (C.parser (S.getValue sec "StartLimitBurst"))
            <*> (C.parser (S.getValue sec "StartLimitInterval"))

    unparser u =
        let
            t = case type_ u of
                TNothing -> mempty
                TSimple exec ->
                    mempty
                        /** ("Type"     , pure "simple")
                        /** ("ExecStart", C.unparser exec)
                TExec exec ->
                    mempty
                        /** ("Type"     , pure "exec")
                        /** ("ExecStart", C.unparser exec)
                TForking pidFile exec ->
                    mempty
                        /** ("Type"     , pure "forking")
                        /** ("PIDFile"  , C.unparser pidFile)
                        /** ("ExecStart", C.unparser exec)
                TOneShot execs ->
                    mempty
                        /** ("Type"     , pure "oneshot")
                        /** ("ExecStart", C.unparser execs)
                TDBus busName exec ->
                    mempty
                        /** ("Type"     , pure "dbus")
                        /** ("BusName"  , C.unparser busName)
                        /** ("ExecStart", C.unparser exec)
                TNotify notifyAccess exec ->
                    mempty
                        /** ("Type"        , pure "notify")
                        /** ("NotifyAccess", C.unparser notifyAccess)
                        /** ("ExecStart"   , C.unparser exec)
                TIdle exec ->
                    mempty
                        /** ("Type"     , pure "idle")
                        /** ("ExecStart", C.unparser exec)
        in  t
                /**? ("User"                 , C.unparser $ user u)
                /**? ("Group"                , C.unparser $ group u)
                /**? ("WorkingDirectory"     , C.unparser $ workingDirectory u)
                /**? ("StandardOutput"       , C.unparser $ standardOutput u)
                /**? ("StandardError"        , C.unparser $ standardError u)
                /**? ("TasksMax"             , C.unparser $ tasksMax u)
                /**? ("Restart"              , C.unparser $ restart u)
                /**? ("PrivateTmp"           , C.unparser $ privateTmp u)
                /**? ("RemainAfterExit"      , C.unparser $ remainAfterExit u)
                /**? ("ExecCondition"        , C.unparser $ execCondition u)
                /**? ("ExecStartPre"         , C.unparser $ execStartPre u)
                /**? ("ExecStartPost"        , C.unparser $ execStartPost u)
                /**? ("ExecReload"           , C.unparser $ execReload u)
                /**? ("ExecStop"             , C.unparser $ execStop u)
                /**? ("ExecStopPost"         , C.unparser $ execStopPost u)
                /**? ("KillMode"             , C.unparser $ killMode u)
                /**? ("ProtectHome"          , C.unparser $ protectHome u)
                /**? ("ProtectSystem"        , C.unparser $ protectSystem u)
                /**? ("CapabilityBoundingSet", C.unparser $ capabilityBoundingSet u)
                /**? ("TimeoutStartSec"      , C.unparser $ timeoutStartSec u)
                /**? ("Delegate"             , C.unparser $ delegate u)
                /**? ("LimitCPU"             , C.unparser $ limitCPU u)
                /**? ("LimitFSIZE"           , C.unparser $ limitFSIZE u)
                /**? ("LimitDATA"            , C.unparser $ limitDATA u)
                /**? ("LimitSTACK"           , C.unparser $ limitSTACK u)
                /**? ("LimitCORE"            , C.unparser $ limitCORE u)
                /**? ("LimitRSS"             , C.unparser $ limitRSS u)
                /**? ("LimitNOFILE"          , C.unparser $ limitNOFILE u)
                /**? ("LimitAS"              , C.unparser $ limitAS u)
                /**? ("LimitNPROC"           , C.unparser $ limitNPROC u)
                /**? ("LimitMEMLOCK"         , C.unparser $ limitMEMLOCK u)
                /**? ("LimitLOCKS"           , C.unparser $ limitLOCKS u)
                /**? ("LimitSIGPENDING"      , C.unparser $ limitSIGPENDING u)
                /**? ("LimitMSGQUEUE"        , C.unparser $ limitMSGQUEUE u)
                /**? ("LimitNICE"            , C.unparser $ limitNICE u)
                /**? ("LimitRTPRIO"          , C.unparser $ limitRTPRIO u)
                /**? ("LimitRTTIME"          , C.unparser $ limitRTTIME u)
                /**? ("StartLimitBurst"      , C.unparser $ startLimitBurst u)
                /**? ("StartLimitInterval", C.unparser $ startLimitInterval u)


-- |A Systemd [Type] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Type=
data Type
    = TNothing
    | TSimple (S.Value Exec)
    | TExec (S.Value Exec)
    | TForking (One PIDFile) (S.Value Exec)
    | TOneShot [S.Value Exec]
    | TDBus (S.Value BusName) (S.Value Exec)
    | TNotify (One NotifyAccess) (S.Value Exec)
    | TIdle (S.Value Exec)
    deriving (Eq, Show)

instance Semigroup Type where
    a           <> TNothing    = a
    TOneShot as <> TOneShot bs = TOneShot (as <> bs)
    _           <> b           = b

instance Monoid Type where
    mempty = TNothing

parseType :: S.Section -> C.ParseResult Type
parseType section = case S.value <$> S.getValue section "Type" of
    [] ->
        TSimple
            <$> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["simple"] ->
        TSimple
            <$> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["exec"] ->
        TExec
            <$> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["forking"] ->
        TForking
            <$> C.parser (S.getValue section "PidFile")
            <*> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["oneshot"] ->
        TOneShot
            <$> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["dbus"] ->
        TDBus
            <$> C.parser (S.getValue section "BusName")
            <*> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["notify"] ->
        TNotify
            <$> C.parser (S.getValue section "NotifyAccess")
            <*> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    ["idle"] ->
        TIdle
            <$> ( C.setErrorField "ExecStart"
                $ C.parser (S.getValue section "ExecStart")
                )
    [other] -> C.ParseError $ C.UnsupportedValue
        other
        ["simple", "exec", "forking", "oneshot", "dbus", "notify", "idle"]
    _ -> C.ParseError $ C.MultipleFound "Type"


-- |Common record for all Exec commands like ExecStart and ExecStop.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#ExecStart=
data Exec = Exec
    { overrideName :: SBool
    , continueOnError :: SBool
    , noEnvironmentVariableSubstitution :: SBool
    , command :: T.Text
    }
    deriving (Eq, Show)

instance Semigroup Exec where
    a <> b = Exec
        { overrideName                      = overrideName a <> overrideName b
        , continueOnError = continueOnError a <> continueOnError b
        , noEnvironmentVariableSubstitution =
            noEnvironmentVariableSubstitution a
                <> noEnvironmentVariableSubstitution b
        , command                           = command a <> command b
        }

instance Monoid Exec where
    mempty = Exec { overrideName                      = mempty
                  , continueOnError                   = mempty
                  , noEnvironmentVariableSubstitution = mempty
                  , command                           = mempty
                  }

instance Data.String.IsString Exec where
    fromString s = mempty { command = T.pack s }

instance C.Config T.Text Exec where
    parser = C.parseText
        (  P.build
                [ P.char '@' $> (mempty { overrideName = sTrue })
                , P.char '-' $> (mempty { continueOnError = sTrue })
                , P.char ':'
                    $> (mempty { noEnvironmentVariableSubstitution = sTrue })
                ]
        <> ((\l -> mempty { command = l }) <$> P.line)
        )

    unparser Exec {..} =
        (if svalue overrideName then "@" else "")
            <> (if svalue continueOnError then "-" else "")
            <> (if svalue noEnvironmentVariableSubstitution then ":" else "")
            <> command


newtype User = User T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last User))

instance Data.String.IsString User where
    fromString = User . T.pack

instance C.Config T.Text User where
    parser = pure . User

    unparser (User t) = t


newtype Group = Group T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last Group))

instance Data.String.IsString Group where
    fromString = Group . T.pack

instance C.Config T.Text Group where
    parser = pure . Group

    unparser (Group t) = t


newtype WorkingDirectory = WorkingDirectory T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last WorkingDirectory))

instance Data.String.IsString WorkingDirectory where
    fromString = WorkingDirectory . T.pack

instance C.Config T.Text WorkingDirectory where
    parser = pure . WorkingDirectory

    unparser (WorkingDirectory t) = t


data Output = OJournal
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last Output))

instance C.Config T.Text Output where
    parser = C.parseText (P.choice [P.chunk "journal" $> OJournal])

    unparser OJournal = "journal"


data Restart
    = RNo
    | RAlways
    | ROnSuccess
    | ROnFailure
    | ROnAbnormal
    | ROnAbort
    | ROnWatchdog
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last Restart))

instance Monoid Restart where
    mempty = RNo

instance C.Config T.Text Restart where
    parser = C.parseText
        (P.choice
            [ P.chunk "no" $> RNo
            , P.chunk "always" $> RAlways
            , P.chunk "on-success" $> ROnSuccess
            , P.chunk "on-failure" $> ROnFailure
            , P.chunk "on-abnormal" $> ROnAbnormal
            , P.chunk "on-abort" $> ROnAbort
            , P.chunk "on-watchdog" $> ROnWatchdog
            ]
        )

    unparser RNo         = "no"
    unparser RAlways     = "always"
    unparser ROnSuccess  = "on-success"
    unparser ROnFailure  = "on-failure"
    unparser ROnAbnormal = "on-abnormal"
    unparser ROnAbort    = "on-abort"
    unparser ROnWatchdog = "on-watchdog"


-- |A convenience type to represent PIDFile=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#PIDFile=
newtype PIDFile = PIDFile T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last PIDFile))

instance C.Config T.Text PIDFile where
    parser = pure . PIDFile

    unparser (PIDFile t) = t


-- |A convenience type to represent RemainAfterExit=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#RemainAfterExit=
newtype RemainAfterExit = RemainAfterExit SBool
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last RemainAfterExit))

instance C.Config T.Text RemainAfterExit where
    parser = fmap RemainAfterExit <$> C.parser

    unparser (RemainAfterExit b) = C.unparser b


-- |A convenience type to represent BusName=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
newtype BusName = BusName T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last BusName))

instance C.Config T.Text BusName where
    parser = pure . BusName

    unparser (BusName t) = t


-- |Represents a NotifyAccess=.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#BusName=
data NotifyAccess
    = NANone
    | NAMain
    | NAExec
    | NAAll
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last NotifyAccess))

instance Monoid NotifyAccess where
    mempty = NANone

instance C.Config T.Text NotifyAccess where
    parser = C.parseText $ P.choice
        [ P.chunk "none" $> NANone
        , P.chunk "main" $> NAMain
        , P.chunk "exec" $> NAExec
        , P.chunk "all" $> NAAll
        ]

    unparser NANone = "none"
    unparser NAMain = "main"
    unparser NAExec = "exec"
    unparser NAAll  = "all"


-- |A convenience type to represent PrivateTmp=.
-- https://www.freedesktop.org/software/systemd/man/systemd.exec.html#PrivateTmp=
newtype PrivateTmp = PrivateTmp SBool
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last PrivateTmp))

instance C.Config T.Text PrivateTmp where
    parser = fmap PrivateTmp <$> C.parser

    unparser (PrivateTmp b) = C.unparser b


data KillMode
    = KMControlGroup
    | KMMixed
    | KMProcess
    | KMNone
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last KillMode))

instance Monoid KillMode where
    mempty = KMNone

instance C.Config T.Text KillMode where
    parser = C.parseText
        (P.choice
            [ P.chunk "control-group" $> KMControlGroup
            , P.chunk "mixed" $> KMMixed
            , P.chunk "process" $> KMProcess
            , P.chunk "none" $> KMNone
            ]
        )

    unparser KMControlGroup = "control-group"
    unparser KMMixed        = "mixed"
    unparser KMProcess      = "process"
    unparser KMNone         = "none"


data ProtectHome
    = PMBool SBool
    | PMReadOnly
    | PMTmpfs
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last ProtectHome))

instance Monoid ProtectHome where
    mempty = PMBool sFalse

instance C.Config T.Text ProtectHome where
    parser = C.parseText
        (P.choice
            [ P.chunk "read-only" $> PMReadOnly
            , P.chunk "tmpfs" $> PMTmpfs
            , PMBool <$> sboolParser
            ]
        )

    unparser PMReadOnly = "read-only"
    unparser PMTmpfs    = "tmpfs"
    unparser (PMBool b) = C.unparser b


data ProtectSystem
    = PSBool SBool
    | PSFull
    | PSStrict
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last ProtectSystem))

instance Monoid ProtectSystem where
    mempty = PSBool sFalse

instance C.Config T.Text ProtectSystem where
    parser = C.parseText
        (P.choice
            [ P.chunk "full" $> PSFull
            , P.chunk "strict" $> PSStrict
            , PSBool <$> sboolParser
            ]
        )

    unparser PSFull     = "full"
    unparser PSStrict   = "strict"
    unparser (PSBool b) = C.unparser b


-- |https://man7.org/linux/man-pages/man7/capabilities.7.html
data Capability
    = CAP_AUDIT_CONTROL
    | CAP_AUDIT_READ
    | CAP_AUDIT_WRITE
    | CAP_BLOCK_SUSPEND
    | CAP_CHOWN
    | CAP_DAC_OVERRIDE
    | CAP_DAC_READ_SEARCH
    | CAP_FOWNER
    | CAP_FSETID
    | CAP_IPC_LOCK
    | CAP_IPC_OWNER
    | CAP_KILL
    | CAP_LEASE
    | CAP_LINUX_IMMUTABLE
    | CAP_MAC_ADMIN
    | CAP_MAC_OVERRIDE
    | CAP_MKNOD
    | CAP_NET_ADMIN
    | CAP_NET_BIND_SERVICE
    | CAP_NET_BROADCAST
    | CAP_NET_RAW
    | CAP_SETGID
    | CAP_SETFCAP
    | CAP_SETPCAP
    | CAP_SETUID
    | CAP_SYS_ADMIN
    | CAP_SYS_BOOT
    | CAP_SYS_CHROOT
    | CAP_SYS_MODULE
    | CAP_SYS_NICE
    | CAP_SYS_PACCT
    | CAP_SYS_PTRACE
    | CAP_SYS_RAWIO
    | CAP_SYS_RESOURCE
    | CAP_SYS_TIME
    | CAP_SYS_TTY_CONFIG
    | CAP_SYSLOG
    | CAP_WAKE_ALARM
    deriving(Show, Eq)

instance C.Config T.Text Capability where
    parser = C.parseText
        (P.choice
            [ "CAP_AUDIT_CONTROL" $> CAP_AUDIT_CONTROL
            , "CAP_AUDIT_READ" $> CAP_AUDIT_READ
            , "CAP_AUDIT_WRITE" $> CAP_AUDIT_WRITE
            , "CAP_BLOCK_SUSPEND" $> CAP_BLOCK_SUSPEND
            , "CAP_CHOWN" $> CAP_CHOWN
            , "CAP_DAC_OVERRIDE" $> CAP_DAC_OVERRIDE
            , "CAP_DAC_READ_SEARCH" $> CAP_DAC_READ_SEARCH
            , "CAP_FOWNER" $> CAP_FOWNER
            , "CAP_FSETID" $> CAP_FSETID
            , "CAP_IPC_LOCK" $> CAP_IPC_LOCK
            , "CAP_IPC_OWNER" $> CAP_IPC_OWNER
            , "CAP_KILL" $> CAP_KILL
            , "CAP_LEASE" $> CAP_LEASE
            , "CAP_LINUX_IMMUTABLE" $> CAP_LINUX_IMMUTABLE
            , "CAP_MAC_ADMIN" $> CAP_MAC_ADMIN
            , "CAP_MAC_OVERRIDE" $> CAP_MAC_OVERRIDE
            , "CAP_MKNOD" $> CAP_MKNOD
            , "CAP_NET_ADMIN" $> CAP_NET_ADMIN
            , "CAP_NET_BIND_SERVICE" $> CAP_NET_BIND_SERVICE
            , "CAP_NET_BROADCAST" $> CAP_NET_BROADCAST
            , "CAP_NET_RAW" $> CAP_NET_RAW
            , "CAP_SETGID" $> CAP_SETGID
            , "CAP_SETFCAP" $> CAP_SETFCAP
            , "CAP_SETPCAP" $> CAP_SETPCAP
            , "CAP_SETUID" $> CAP_SETUID
            , "CAP_SYS_ADMIN" $> CAP_SYS_ADMIN
            , "CAP_SYS_BOOT" $> CAP_SYS_BOOT
            , "CAP_SYS_CHROOT" $> CAP_SYS_CHROOT
            , "CAP_SYS_MODULE" $> CAP_SYS_MODULE
            , "CAP_SYS_NICE" $> CAP_SYS_NICE
            , "CAP_SYS_PACCT" $> CAP_SYS_PACCT
            , "CAP_SYS_PTRACE" $> CAP_SYS_PTRACE
            , "CAP_SYS_RAWIO" $> CAP_SYS_RAWIO
            , "CAP_SYS_RESOURCE" $> CAP_SYS_RESOURCE
            , "CAP_SYS_TIME" $> CAP_SYS_TIME
            , "CAP_SYS_TTY_CONFIG" $> CAP_SYS_TTY_CONFIG
            , "CAP_SYSLOG" $> CAP_SYSLOG
            , "CAP_WAKE_ALARM" $> CAP_WAKE_ALARM
            ]
        )

    unparser CAP_AUDIT_CONTROL    = "CAP_AUDIT_CONTROL"
    unparser CAP_AUDIT_READ       = "CAP_AUDIT_READ"
    unparser CAP_AUDIT_WRITE      = "CAP_AUDIT_WRITE"
    unparser CAP_BLOCK_SUSPEND    = "CAP_BLOCK_SUSPEND"
    unparser CAP_CHOWN            = "CAP_CHOWN"
    unparser CAP_DAC_OVERRIDE     = "CAP_DAC_OVERRIDE"
    unparser CAP_DAC_READ_SEARCH  = "CAP_DAC_READ_SEARCH"
    unparser CAP_FOWNER           = "CAP_FOWNER"
    unparser CAP_FSETID           = "CAP_FSETID"
    unparser CAP_IPC_LOCK         = "CAP_IPC_LOCK"
    unparser CAP_IPC_OWNER        = "CAP_IPC_OWNER"
    unparser CAP_KILL             = "CAP_KILL"
    unparser CAP_LEASE            = "CAP_LEASE"
    unparser CAP_LINUX_IMMUTABLE  = "CAP_LINUX_IMMUTABLE"
    unparser CAP_MAC_ADMIN        = "CAP_MAC_ADMIN"
    unparser CAP_MAC_OVERRIDE     = "CAP_MAC_OVERRIDE"
    unparser CAP_MKNOD            = "CAP_MKNOD"
    unparser CAP_NET_ADMIN        = "CAP_NET_ADMIN"
    unparser CAP_NET_BIND_SERVICE = "CAP_NET_BIND_SERVICE"
    unparser CAP_NET_BROADCAST    = "CAP_NET_BROADCAST"
    unparser CAP_NET_RAW          = "CAP_NET_RAW"
    unparser CAP_SETGID           = "CAP_SETGID"
    unparser CAP_SETFCAP          = "CAP_SETFCAP"
    unparser CAP_SETPCAP          = "CAP_SETPCAP"
    unparser CAP_SETUID           = "CAP_SETUID"
    unparser CAP_SYS_ADMIN        = "CAP_SYS_ADMIN"
    unparser CAP_SYS_BOOT         = "CAP_SYS_BOOT"
    unparser CAP_SYS_CHROOT       = "CAP_SYS_CHROOT"
    unparser CAP_SYS_MODULE       = "CAP_SYS_MODULE"
    unparser CAP_SYS_NICE         = "CAP_SYS_NICE"
    unparser CAP_SYS_PACCT        = "CAP_SYS_PACCT"
    unparser CAP_SYS_PTRACE       = "CAP_SYS_PTRACE"
    unparser CAP_SYS_RAWIO        = "CAP_SYS_RAWIO"
    unparser CAP_SYS_RESOURCE     = "CAP_SYS_RESOURCE"
    unparser CAP_SYS_TIME         = "CAP_SYS_TIME"
    unparser CAP_SYS_TTY_CONFIG   = "CAP_SYS_TTY_CONFIG"
    unparser CAP_SYSLOG           = "CAP_SYSLOG"
    unparser CAP_WAKE_ALARM       = "CAP_WAKE_ALARM"

data InclusionCapabilities a
    = IncludedCapabilities a
    | ExcludedCapabilities a
    deriving(Show, Eq, Functor)

instance C.Config T.Text a => C.Config T.Text (InclusionCapabilities a) where
    parser t = constructor t >>= embeddedParser
      where
        constructor :: T.Text -> C.ParseResult (InclusionCapabilities T.Text)
        constructor = C.parseText
            (P.optional "~" >>= \case
                Nothing -> IncludedCapabilities <$> P.line
                Just _  -> ExcludedCapabilities <$> P.line
            )

        embeddedParser
            :: C.Config T.Text a
            => InclusionCapabilities T.Text
            -> C.ParseResult (InclusionCapabilities a)
        embeddedParser v = case fmap C.parser v of
            IncludedCapabilities r -> IncludedCapabilities <$> r
            ExcludedCapabilities r -> ExcludedCapabilities <$> r

    unparser (IncludedCapabilities v) = C.unparser v
    unparser (ExcludedCapabilities v) = "~" <> C.unparser v

type CapabilitiesSet = ResettableList (InclusionCapabilities (Words Capability))
