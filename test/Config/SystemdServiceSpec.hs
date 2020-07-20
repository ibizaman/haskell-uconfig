{-# LANGUAGE QuasiQuotes #-}

module Config.SystemdServiceSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           Text.RawString.QQ

import           Config.SystemdService
import qualified Config                        as C
import qualified Data.Text                     as T
import qualified Syntax                        as S
import qualified Syntax.XDGDesktop             as XDGDesktop
import           Syntax                         ( (/*)
                                                , (/**)
                                                )


spec :: H.Spec
spec = do
    H.describe "parse sbool" $ do
        H.it "parses false-byes"
            $              C.parser ("no" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess (setType BYes sFalse)
        H.it "parses true-byes"
            $              C.parser ("yes" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess (setType BYes sTrue)
        H.it "parses false-bnum"
            $              C.parser ("0" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess (setValue False $ setType BNum sFalse)
        H.it "parses true-bnum"
            $              C.parser ("1" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess (setType BNum sTrue)
    H.describe "parse exec" $ do
        H.it "parses empty command"
            $              C.parser ("" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess (mempty :: Exec)
        H.it "parses simple command"
            $              C.parser ("xyz" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess ((mempty :: Exec) { command = "xyz" })
        H.it "parses simple command with spaces"
            $              C.parser ("x y z" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Exec) { command = "x y z" })
        H.it "parses command with options"
            $              C.parser ("@-xy@-z" :: T.Text)
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Exec) { command         = "xy@-z"
                                                 , overrideName    = sTrue
                                                 , continueOnError = sTrue
                                                 }
                               )
    H.describe "parseService" $ do
        H.it "fails if wrong Type"
            $              (C.parser :: S.Section -> C.ParseResult Service)
                               (mempty /** ("Type", ["xyz"]))
            `HPP.shouldBe` C.ParseError
                               (C.UnsupportedValue
                                   "xyz"
                                   [ "simple"
                                   , "exec"
                                   , "forking"
                                   , "oneshot"
                                   , "dbus"
                                   , "notify"
                                   , "idle"
                                   ]
                               )
        H.it "fails if Type is right but no ExecStart"
            $              (C.parser :: S.Section -> C.ParseResult Service)
                               (mempty /** ("Type", ["simple"]))
            `HPP.shouldBe` C.ParseError (C.FieldNotFound "ExecStart")
        H.it "no Type is same as Type=simple"
            $ (C.parser :: S.Section -> C.ParseResult Service) mempty
            `HPP.shouldBe` C.ParseError (C.FieldNotFound "ExecStart")
        H.it "succeeds with Type and ExecStart"
            $              (C.parser :: S.Section -> C.ParseResult Service)
                               (mempty /** ("Type", ["simple"]) /** ("ExecStart", ["mycmd"]))
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Service) { type_ = TSimple "mycmd" })
        H.it "succeeds with Type and ExecStart with spaces"
            $              (C.parser :: S.Section -> C.ParseResult Service)
                               (   mempty
                               /** ("Type"     , ["simple"])
                               /** ("ExecStart", ["m y cm d"])
                               )
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Service) { type_ = TSimple "m y cm d"
                                                    }
                               )
    H.describe "parseUnit" $ do
        H.it "with before"
            $              (C.parser :: S.Section -> C.ParseResult Unit)
                               (mempty /** ("Before", ["one two"]) /** ("Before", ["three"]))
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Unit)
                                   { before =
                                       ResettableList
                                           (   S.newValue
                                           <$> [ ListElem ["one", "two"]
                                               , ListElem ["three"]
                                               ]
                                           )
                                   }
                               )
        H.it "with reset before"
            $              (C.parser :: S.Section -> C.ParseResult Unit)
                               (   mempty
                               /** ("Before", ["one two"])
                               /** ("Before", [""])
                               /** ("Before", ["four"])
                               )
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Unit)
                                   { before = ResettableList
                                       (   S.newValue
                                       <$> [ ListElem ["one", "two"]
                                           , ListReset
                                           , ListElem ["four"]
                                           ]
                                       )
                                   }
                               )
        H.it "with description"
            $              (C.parser :: S.Section -> C.ParseResult Unit)
                               (mempty /** ("Description", ["my service"]))
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Unit)
                                   { description = Value "my service"
                                   }
                               )
    H.describe "parseInstall" $ do
        H.it "empty"
            $ (C.parser :: S.Section -> C.ParseResult Install) (mempty)
            `HPP.shouldBe` C.ParseSuccess (mempty :: Install)
        H.it "wantedBy"
            $              (C.parser :: S.Section -> C.ParseResult Install)
                               (mempty /** ("WantedBy", ["default.target"]))
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Install)
                                   { wantedBy =
                                       ResettableList
                                           [ S.newValue
                                                 $ ListElem ["default.target"]
                                           ]
                                   }
                               )
        H.it "wantedBy and requiredBy"
            $              (C.parser :: S.Section -> C.ParseResult Install)
                               (   mempty
                               /** ("WantedBy"  , ["default.target"])
                               /** ("RequiredBy", ["other.target"])
                               /** ("RequiredBy", ["4"])
                               /** ("WantedBy"  , ["1 2 3"])
                               )
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Install)
                                   { wantedBy   = ResettableList
                                       [ S.newValue
                                           $ ListElem ["default.target"]
                                       , S.newValue $ ListElem ["1", "2", "3"]
                                       ]
                                   , requiredBy = ResettableList
                                       [ S.newValue $ ListElem ["other.target"]
                                       , S.newValue $ ListElem ["4"]
                                       ]
                                   }
                               )
    H.describe "parse full service" $ do
        H.it "empty headers"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService) mempty
            `HPP.shouldBe` C.ParseError (C.FieldNotFound "ExecStart")
        H.it "with description"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Unit"
                     , (mempty /** ("Description", ["my service"]))
                     )
                  /* (Just "Service", (mempty /** ("ExecStart", ["my cmd"])))
                  )
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                                   { description = Value
                                                                       "my service"
                                                   }
                                   , service = mempty { type_ = TSimple "my cmd"
                                                      }
                                   }
                               )
        H.it "with description and type"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Unit"
                     , (mempty /** ("Description", ["my service"]))
                     )
                  /* (Just "Service", (mempty /** ("ExecStart", ["my cmd"])))
                  )
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: SystemdService)
                                   { unit    = mempty
                                                   { description = Value
                                                                       "my service"
                                                   }
                                   , service = mempty { type_ = TSimple "my cmd"
                                                      }
                                   }
                               )
        H.it "with service"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Service"
                     , (   mempty
                       /** ("User"            , ["aria2"])
                       /** ("Group"           , ["aria2"])
                       /** ("WorkingDirectory", ["/opt/AriaNg-1.1.1"])
                       /** ( "ExecStart"
                           , ["/usr/bin/darkhttpd . --port 6810"]
                           )
                       )
                     )
                  )
            `HPP.shouldBe` C.ParseSuccess (mempty :: SystemdService)
                               { service = mempty
                                   { type_            = TSimple

                                       "/usr/bin/darkhttpd . --port 6810"
                                   , user             = Value "aria2"
                                   , group            = Value "aria2"
                                   , workingDirectory = Value
                                                            "/opt/AriaNg-1.1.1"
                                   }
                               }
        H.it "with service and tasksmax=10"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Service"
                     , (   mempty
                       /** ("TasksMax", ["10"])
                       /** ( "ExecStart"
                           , ["/usr/bin/darkhttpd . --port 6810"]
                           )
                       )
                     )
                  )
            `HPP.shouldBe` C.ParseSuccess (mempty :: SystemdService)
                               { service = mempty
                                   { type_    = TSimple
                                       "/usr/bin/darkhttpd . --port 6810"
                                   , tasksMax = Value $ S.newValue $ TasksMax 10
                                   }
                               }
        H.it "with service and tasksmax=infinity"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Service"
                     , (   mempty
                       /** ("TasksMax", ["infinity"])
                       /** ( "ExecStart"
                           , ["/usr/bin/darkhttpd . --port 6810"]
                           )
                       )
                     )
                  )
            `HPP.shouldBe` C.ParseSuccess (mempty :: SystemdService)
                               { service = mempty
                                   { type_    = TSimple
                                       "/usr/bin/darkhttpd . --port 6810"
                                   , tasksMax = Value
                                                $ S.newValue
                                                $ TasksMaxInfinity
                                   }
                               }
        H.it "with service and disabled tasksmax=infinity"
            $ (C.parser :: S.XDGDesktop -> C.ParseResult SystemdService)
                  (  mempty
                  /* ( Just "Service"
                     , (   mempty
                       /** ("TasksMax", [S.setEnabled False "infinity"])
                       /** ( "ExecStart"
                           , ["/usr/bin/darkhttpd . --port 6810"]
                           )
                       )
                     )
                  )
            `HPP.shouldBe` C.ParseSuccess (mempty :: SystemdService)
                               { service = mempty
                                   { type_    = TSimple
                                       "/usr/bin/darkhttpd . --port 6810"
                                   , tasksMax = Value
                                                $ S.setEnabled False
                                                $ S.newValue
                                                $ TasksMaxInfinity
                                   }
                               }
    H.describe "full-fledge services" $ do
        fullRoundTrip
            "aria2 web service"
            [r|[Unit]
Description=Aria2 Web Service
After=network.target

[Service]
User=aria2
Group=aria2
WorkingDirectory=/opt/AriaNg-1.1.1
Type=simple
ExecStart=/usr/bin/darkhttpd . --port 6810

[Install]
WantedBy=default.target|]
            (mempty
                { unit    = mempty
                    { description = Value "Aria2 Web Service"
                    , after       = ResettableList
                                        [S.newValue $ ListElem ["network.target"]]
                    }
                , service = mempty
                    { user             = Value "aria2"
                    , group            = Value "aria2"
                    , workingDirectory = Value "/opt/AriaNg-1.1.1"
                    , type_ = TSimple "/usr/bin/darkhttpd . --port 6810"
                    }
                , install = mempty
                    { wantedBy = ResettableList
                                     [S.newValue $ ListElem ["default.target"]]
                    }
                }
            )
        fullRoundTrip
            "laptop mode tools"
            [r|[Unit]
Description=Laptop Mode Tools
Documentation=man:laptop_mode(8) man:laptop-mode.conf(8)
Documentation=http://github.com/rickysarraf/laptop-mode-tools

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/enabled
ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/state
ExecStart=/usr/bin/laptop_mode init force
ExecStop=/usr/bin/laptop_mode init stop
ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/enabled
ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/state
ExecReload=/usr/bin/laptop_mode auto
StandardOutput=journal
StandardError=journal
TasksMax=infinity

[Install]
WantedBy=multi-user.target|]
            (mempty
                { unit    = mempty
                    { description   = Value "Laptop Mode Tools"
                    , documentation = List
                        [ S.newValue
                            [ DocMan "laptop_mode(8)"
                            , DocMan "laptop-mode.conf(8)"
                            ]
                        , S.newValue
                            [DocHTTP "github.com/rickysarraf/laptop-mode-tools"]
                        ]
                    }
                , service = mempty
                    { type_ = TOneShot ["/usr/bin/laptop_mode init force"]
                    , remainAfterExit = Value $ S.newValue $ RemainAfterExit
                                            (setType BYes sTrue)
                    , execStartPre    =
                        [ "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                        , "/bin/rm -f /var/run/laptop-mode-tools/state"
                        ]
                    , execStop        = ["/usr/bin/laptop_mode init stop"]
                    , execStopPost    =
                        [ "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                        , "/bin/rm -f /var/run/laptop-mode-tools/state"
                        ]
                    , execReload      = Value "/usr/bin/laptop_mode auto"
                    , standardOutput  = Value $ S.newValue OJournal
                    , standardError   = Value $ S.newValue OJournal
                    , tasksMax        = Value $ S.newValue TasksMaxInfinity
                    }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem ["multi-user.target"]]
                    }
                }
            )
        fullRoundTrip
            "network time service"
            [r|[Unit]
Description=Network Time Service
After=network.target nss-lookup.target
Conflicts=systemd-timesyncd.service

[Service]
Type=forking
PrivateTmp=true
ExecStart=/usr/bin/ntpd -g -u ntp:ntp
Restart=always

[Install]
WantedBy=multi-user.target|]
            (mempty
                { unit    = mempty
                    { description = Value "Network Time Service"
                    , after       =
                        ResettableList
                            [ S.newValue $ ListElem
                                  ["network.target", "nss-lookup.target"]
                            ]
                    , conflicts   = ResettableList
                        [S.newValue $ ListElem ["systemd-timesyncd.service"]]
                    }
                , service = mempty
                                { type_      = TForking
                                                   Empty
                                                   "/usr/bin/ntpd -g -u ntp:ntp"
                                , privateTmp = Value
                                               $ S.newValue
                                               $ PrivateTmp
                                               $ setType BTrue sTrue
                                , restart    = Value $ S.newValue $ RAlways
                                }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem ["multi-user.target"]]
                    }
                }
            )
        fullRoundTrip
            "networking for netctl profile %I"
            [r|[Unit]
Description=Networking for netctl profile %I
Documentation=man:netctl.profile(5)
After=network-pre.target
Before=network.target netctl.service
Wants=network.target

[Service]
Type=notify
NotifyAccess=exec
RemainAfterExit=yes
ExecStart=/usr/lib/netctl/network start %I
ExecStop=/usr/lib/netctl/network stop %I|]
            (mempty
                { unit    = mempty
                    { description   = Value "Networking for netctl profile %I"
                    , documentation = List
                        [S.newValue [DocMan "netctl.profile(5)"]]
                    , after         = ResettableList
                                          [S.newValue $ ListElem ["network-pre.target"]]
                    , before        =
                        ResettableList
                            [ S.newValue $ ListElem
                                  ["network.target", "netctl.service"]
                            ]
                    , wants         = ResettableList
                                          [S.newValue $ ListElem ["network.target"]]
                    }
                , service = mempty
                                { type_ = TNotify
                                              (Value $ S.newValue NAExec)
                                              "/usr/lib/netctl/network start %I"
                                , remainAfterExit = Value
                                                    $ S.newValue
                                                    $ RemainAfterExit
                                                    $ setType BYes sTrue
                                , execStop = ["/usr/lib/netctl/network stop %I"]
                                }
                }
            )

fullRoundTrip :: String -> T.Text -> SystemdService -> H.Spec
fullRoundTrip name t v = do
    let intermediate = (C.parser t) :: C.ParseResult S.XDGDesktop

        follow       = case intermediate of
            C.ParseError   _ -> id
            C.ParseSuccess i -> \x -> x `XDGDesktop.followOrderFrom` i

        p :: C.ParseResult SystemdService
        p = intermediate >>= C.parser

        g :: SystemdService -> T.Text
        g x = C.unparser $ follow $ ((C.unparser x) :: S.XDGDesktop)

    H.it (name <> " - parse") $ p `HPP.shouldBe` C.ParseSuccess v
    H.it (name <> " - generate") $ g v `HPP.shouldBe` t
