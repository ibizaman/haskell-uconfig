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

The Config.SystemdService module provides a function to parse Systemd
Service files.

https://www.freedesktop.org/software/systemd/man/systemd.service.html
-}
module Config.SystemdService
    ( EmptyDefault(..)
    , SystemdService(..)
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
    , TasksMax(..)
    , PrivateTmp(..)
    , Restart(..)
    )
where


import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( ($>) )
import qualified Data.String
import qualified Data.Text                     as T
import           Data.Semigroup                 ( Semigroup(..)
                                                , First(..)
                                                , Last(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Generic.Data                   ( Generically(..) )
import qualified Parser                        as P
import qualified Config                        as C
import qualified Syntax                        as S
import           Syntax                         ( (/*)
                                                , (/**)
                                                )


-- |Value that can be empty. It's Maybe with a different Semigroup
-- implementation.
data EmptyDefault a = Empty | Value a
  deriving(Eq, Show)

instance Semigroup (EmptyDefault x) where
    Value a <> _       = Value a
    _       <> Value b = Value b
    _       <> _       = Empty

instance Monoid (EmptyDefault x) where
    mempty = Empty

instance Functor EmptyDefault where
    fmap f  (Value m) = Value (f m)
    fmap _f Empty     = Empty

instance Applicative EmptyDefault where
    pure = Value
    Value f <*> m  = fmap f m
    Empty   <*> _m = Empty

instance C.ToList EmptyDefault where
    toList Empty      = []
    toList (Value v') = [v']


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
            /* (Just "Unit"   , C.unparser (unit s))
            /* (Just "Install", C.unparser (install s))
            /* (Just "Service", C.unparser (service s))

-- |A Systemd [Unit] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BUnit%5D%20Section%20Options
data Unit = Unit
  { description :: EmptyDefault (S.Value Description)
  , documentation :: [S.Value Documentation]
  , before :: [S.Value Target]
  , after :: [S.Value Target]
  , wants :: [S.Value Target]
  , requires :: [S.Value Target]
  , conflicts :: [S.Value Target]
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically Unit)

instance C.Config S.Section Unit where
    parser sec =
        Unit
            <$> (C.parseOneOptional C.parser "Description" sec)
            <*> (C.parseMultiple C.parser "Documentation" sec)
            <*> (C.parseMultiple C.parser "Before" sec)
            <*> (C.parseMultiple C.parser "After" sec)
            <*> (C.parseMultiple C.parser "Wants" sec)
            <*> (C.parseMultiple C.parser "Requires" sec)
            <*> (C.parseMultiple C.parser "Conflicts" sec)

    unparser u =
        mempty
            /** ("Description"  , C.unparser $ description u)
            /** ("Documentation", C.unparser $ documentation u)
            /** ("Before"       , C.unparser $ before u)
            /** ("After"        , C.unparser $ after u)
            /** ("Wants"        , C.unparser $ wants u)
            /** ("Requires"     , C.unparser $ requires u)
            /** ("Conflicts"    , C.unparser $ conflicts u)

newtype Description = Description T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Description)

instance Data.String.IsString Description where
    fromString = Description . T.pack

instance C.Config T.Text Description where
    parser = fmap Description . C.parseText P.words

    unparser (Description d) = d


newtype Documentation = Documentation T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Documentation)

instance Data.String.IsString Documentation where
    fromString = Documentation . T.pack

instance C.Config T.Text Documentation where
    parser = fmap Documentation
        . C.parseText (C.plain <$> C.spaced (C.quoted P.word))

    unparser (Documentation d) = d


newtype Target = Target T.Text
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Target)

instance Data.String.IsString Target where
    fromString = Target . T.pack

instance C.Config T.Text Target where
    parser = fmap Target . C.parseText (C.plain <$> C.spaced (C.quoted P.word))

    unparser (Target t) = t


-- |A Systemd [Install] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.unit.html#%5BInstall%5D%20Section%20Options
data Install = Install
    { wantedBy :: [S.Value Target]
    , requiredBy :: [S.Value Target]
    }
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Install)

instance C.Config S.Section Install where
    parser sec =
        Install
            <$> (C.parseMultiple C.parser "WantedBy" sec)
            <*> (C.parseMultiple C.parser "RequiredBy" sec)

    unparser u =
        mempty
            /** ("WantedBy"  , C.unparser $ wantedBy u)
            /** ("RequiredBy", C.unparser $ requiredBy u)


-- |A Systemd [Service] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Options
-- https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html#TasksMax=N
data Service = Service
    { execCondition :: [S.Value Exec]
    , execStartPre :: [S.Value Exec]
    , execStartPost :: [S.Value Exec]
    , type_ :: Type
    , execReload :: EmptyDefault (S.Value Exec)
    , execStop :: [S.Value Exec]
    , execStopPost :: [S.Value Exec]
    , remainAfterExit :: EmptyDefault (S.Value RemainAfterExit)
    , user :: EmptyDefault (S.Value User)
    , group :: EmptyDefault (S.Value Group)
    , workingDirectory :: EmptyDefault (S.Value WorkingDirectory)
    , standardOutput :: EmptyDefault (S.Value Output)
    , standardError :: EmptyDefault (S.Value Output)
    , tasksMax :: EmptyDefault (S.Value TasksMax)
    , restart :: EmptyDefault (S.Value Restart)
    , privateTmp :: EmptyDefault (S.Value PrivateTmp)
    }
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically Service)

instance C.Config S.Section Service where
    parser sec =
        Service
            <$> (C.parseMultiple C.parser "ExecCondition" sec)
            <*> (C.parseMultiple C.parser "ExecStartPre" sec)
            <*> (C.parseMultiple C.parser "ExecstartPost" sec)
            <*> parseType sec
            <*> (C.parseOneOptional C.parser "ExecReload" sec)
            <*> (C.parseMultiple C.parser "ExecStop" sec)
            <*> (C.parseMultiple C.parser "ExecStopPost" sec)
            <*> (C.parseOneOptional C.parser "RemainAfterExit" sec)
            <*> (C.parseOneOptional C.parser "User" sec)
            <*> (C.parseOneOptional C.parser "Group" sec)
            <*> (C.parseOneOptional C.parser "WorkingDirectory" sec)
            <*> (C.parseOneOptional C.parser "StandardOutput" sec)
            <*> (C.parseOneOptional C.parser "StandardError" sec)
            <*> (C.parseOneOptional C.parser "TasksMax" sec)
            <*> (C.parseOneOptional C.parser "Restart" sec)
            <*> (C.parseOneOptional C.parser "PrivateTmp" sec)

    unparser u =
        let
            t = case type_ u of
                TNothing -> mempty
                TSimple exec ->
                    mempty
                        /** ("Type"     , pure "simple")
                        /** ("ExecStart", C.unparser $ Just exec)
                TExec exec ->
                    mempty
                        /** ("Type"     , pure "exec")
                        /** ("ExecStart", C.unparser $ Just exec)
                TForking pidFile exec ->
                    mempty
                        /** ("Type"     , pure "forking")
                        /** ("PIDFile"  , C.unparser pidFile)
                        /** ("ExecStart", C.unparser $ Just exec)
                TOneShot execs -> mempty /** ("ExecStart", C.unparser execs)
                TDBus busName exec ->
                    mempty
                        /** ("Type"     , pure "dbus")
                        /** ("BusName"  , C.unparser $ Just busName)
                        /** ("ExecStart", C.unparser $ Just exec)
                TNotify notifyAccess exec ->
                    mempty
                        /** ("Type"        , pure "notify")
                        /** ("NotifyAccess", C.unparser $ Just notifyAccess)
                        /** ("ExecStart"   , C.unparser $ Just exec)
                TIdle exec ->
                    mempty
                        /** ("Type"     , pure "idle")
                        /** ("ExecStart", C.unparser $ Just exec)
        in  t
                /** ("User"            , C.unparser $ user u)
                /** ("Group"           , C.unparser $ group u)
                /** ("WorkingDirectory", C.unparser $ workingDirectory u)
                /** ("StandardOutput"  , C.unparser $ standardOutput u)
                /** ("StandardError"   , C.unparser $ standardError u)
                /** ("TasksMax"        , C.unparser $ tasksMax u)
                /** ("Restart"         , C.unparser $ restart u)
                /** ("PrivateTmp"      , C.unparser $ privateTmp u)
                /** ("RemainAfterExit" , C.unparser $ remainAfterExit u)
                /** ("ExecCondition"   , C.unparser $ execCondition u)
                /** ("ExecStartPre"    , C.unparser $ execStartPre u)
                /** ("ExecStartPost"   , C.unparser $ execStartPost u)
                /** ("ExecReload="     , C.unparser $ execReload u)
                /** ("ExecStop="       , C.unparser $ execStop u)
                /** ("ExecStopPost="   , C.unparser $ execStopPost u)


-- |A Systemd [Type] record.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#Type=
data Type
    = TNothing
    | TSimple (S.Value Exec)
    | TExec (S.Value Exec)
    | TForking (EmptyDefault (S.Value PIDFile)) (S.Value Exec)
    | TOneShot [S.Value Exec]
    | TDBus (S.Value BusName) (S.Value Exec)
    | TNotify (S.Value NotifyAccess) (S.Value Exec)
    | TIdle (S.Value Exec)
    deriving (Eq, Show)

instance Semigroup Type where
    a           <> TNothing    = a
    TOneShot as <> TOneShot bs = TOneShot (as <> bs)
    _           <> b           = b

instance Monoid Type where
    mempty = TNothing

parseType :: S.Section -> C.ParseResult Type
parseType section =
    fmap S.value <$> C.parseOneOptional pure "Type" section >>= \case
        Nothing       -> TSimple <$> C.parseOne C.parser "ExecStart" section
        Just "simple" -> TSimple <$> C.parseOne C.parser "ExecStart" section
        Just "exec"   -> TExec <$> C.parseOne C.parser "ExecStart" section
        Just "forking" ->
            TForking
                <$> C.parseOneOptional C.parser "PidFile" section
                <*> C.parseOne C.parser "ExecStart" section
        Just "oneshot" ->
            TOneShot <$> C.parseMultiple C.parser "ExecStart" section
        Just "dbus" ->
            TDBus
                <$> C.parseOne C.parser "BusName" section
                <*> C.parseOne C.parser "ExecStart" section
        Just "notify" ->
            TNotify
                <$> C.parseOne C.parser "NotifyAccess" section
                <*> C.parseOne C.parser "ExecStart" section
        Just "idle" -> TIdle <$> C.parseOne C.parser "ExecStart" section
        Just other  -> C.ParseError $ C.UnsupportedValue
            other
            ["simple", "exec", "forking", "oneshot", "dbus", "notify", "idle"]


-- |Common record for all Exec commands like ExecStart and ExecStop.
-- https://www.freedesktop.org/software/systemd/man/systemd.service.html#ExecStart=
data Exec = Exec
    { overrideName :: Bool
    , continueOnError :: Bool
    , noEnvironmentVariableSubstitution :: Bool
    , command :: T.Text
    }
    deriving (Eq, Show)

instance Semigroup Exec where
    a <> b = Exec
        { overrideName                      = overrideName a || overrideName b
        , continueOnError = continueOnError a || continueOnError b
        , noEnvironmentVariableSubstitution =
            noEnvironmentVariableSubstitution a
                || noEnvironmentVariableSubstitution b
        , command                           = command a <> command b
        }

instance Monoid Exec where
    mempty = Exec { overrideName                      = False
                  , continueOnError                   = False
                  , noEnvironmentVariableSubstitution = False
                  , command                           = mempty
                  }

instance Data.String.IsString Exec where
    fromString s = mempty { command = T.pack s }

instance C.Config T.Text Exec where
    parser = C.parseText
        (  P.build
                [ P.char '@' $> (mempty { overrideName = True })
                , P.char '-' $> (mempty { continueOnError = True })
                , P.char ':'
                    $> (mempty { noEnvironmentVariableSubstitution = True })
                ]
        <> ((\l -> mempty { command = l }) <$> P.line)
        )

    unparser Exec {..} =
        (if overrideName then "@" else "")
            <> (if continueOnError then "-" else "")
            <> (if noEnvironmentVariableSubstitution then ":" else "")
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


data TasksMax = TasksMax Int | TasksMaxInfinity
    deriving (Eq, Show, Generic)
    deriving (Semigroup) via (Generically (Last TasksMax))

instance Monoid TasksMax where
    mempty = TasksMax 0

instance C.Config T.Text TasksMax where
    parser = C.parseText
        ((TasksMax <$> P.number) <|> (P.chunk "infinity" $> TasksMaxInfinity))

    unparser (TasksMax i)     = T.pack $ show i
    unparser TasksMaxInfinity = "infinity"


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
newtype RemainAfterExit = RemainAfterExit Bool
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last RemainAfterExit))

instance C.Config T.Text RemainAfterExit where
    parser = fmap RemainAfterExit <$> C.parseBool

    unparser (RemainAfterExit b) = C.unparseBool b


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
newtype PrivateTmp = PrivateTmp Bool
    deriving (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (Last PrivateTmp))

instance C.Config T.Text PrivateTmp where
    parser = fmap PrivateTmp <$> C.parseBool

    unparser (PrivateTmp b) = C.unparseBool b
