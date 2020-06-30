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
import           Syntax                         ( (/*)
                                                , (/**)
                                                )


spec :: H.Spec
spec = do
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
                                                 , overrideName    = True
                                                 , continueOnError = True
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
                                   { before = S.newValue
                                                  <$> [ ["one", "two"]
                                                      , ["three"]
                                                      ]
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
                                   { wantedBy = [S.newValue ["default.target"]]
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
                                   { wantedBy   = S.newValue
                                                      <$> [ ["default.target"]
                                                          , ["1", "2", "3"]
                                                          ]
                                   , requiredBy = S.newValue
                                                      <$> [ ["other.target"]
                                                          , ["4"]
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
--     H.describe "full-fledge services" $ do
--         H.it "aria2 web service"
--             $ C.parser [r|[Unit]
-- Description=Aria2 Web Service
-- After=network.target
--
-- [Service]
-- User=aria2
-- Group=aria2
-- WorkingDirectory=/opt/AriaNg-1.1.1
-- ExecStart=/usr/bin/darkhttpd . --port 6810
--
-- [Install]
-- WantedBy=default.target
-- |]
--             `HPP.shouldBe` C.ParseSuccess
--                                ((mempty :: SystemdService)
--                                    { unit    = mempty
--                                                    { description =
--                                                        "Aria2 Web Service"
--                                                    , after = ["network.target"]
--                                                    }
--                                    , service = mempty
--                                        { user             = "aria2"
--                                        , group            = "aria2"
--                                        , workingDirectory = "/opt/AriaNg-1.1.1"
--                                        , type_            = TSimple
--                                            "/usr/bin/darkhttpd . --port 6810"
--                                        }
--                                    , install = mempty
--                                                    { wantedBy =
--                                                        ["default.target"]
--                                                    }
--                                    }
--                                )
--         H.it "laptop mode tools"
--             $ C.parser [r|[Unit]
-- Description=Laptop Mode Tools
-- Documentation=man:laptop_mode(8) man:laptop-mode.conf(8)
-- Documentation=http://github.com/rickysarraf/laptop-mode-tools
--
-- [Service]
-- Type=oneshot
-- RemainAfterExit=yes
-- ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/enabled
-- ExecStartPre=/bin/rm -f /var/run/laptop-mode-tools/state
-- ExecStart=/usr/bin/laptop_mode init force
-- ExecStop=/usr/bin/laptop_mode init stop
-- ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/enabled
-- ExecStopPost=/bin/rm -f /var/run/laptop-mode-tools/state
-- ExecReload=/usr/bin/laptop_mode auto
-- StandardOutput=journal
-- StandardError=journal
-- TasksMax=infinity
--
-- [Install]
-- WantedBy=multi-user.target
-- |]
--             `HPP.shouldBe` C.ParseSuccess
--                                ((mempty :: SystemdService)
--                                    { unit    = mempty
--                                        { description   = "Laptop Mode Tools"
--                                        , documentation =
--
--                                            [ "man:laptop_mode(8)"
--                                            , "man:laptop-mode.conf(8)"
--                                            , "http://github.com/rickysarraf/laptop-mode-tools"
--                                            ]
--                                        }
--                                    , service = mempty
--                                        { type_           = TOneShot
--                                            ["/usr/bin/laptop_mode init force"]
--                                        , remainAfterExit =
--                                            Value $ S.newValue $ RemainAfterExit
--                                                True
--                                        , execStartPre    =
--                                            [ "/bin/rm -f /var/run/laptop-mode-tools/enabled"
--                                            , "/bin/rm -f /var/run/laptop-mode-tools/state"
--                                            ]
--                                        , execStop        =
--                                            ["/usr/bin/laptop_mode init stop"]
--                                        , execStopPost    =
--                                            [ "/bin/rm -f /var/run/laptop-mode-tools/enabled"
--                                            , "/bin/rm -f /var/run/laptop-mode-tools/state"
--                                            ]
--                                        , execReload      =
--                                            "/usr/bin/laptop_mode auto"
--                                        , standardOutput  = Value
--                                            $ S.newValue OJournal
--                                        , standardError   = Value
--                                            $ S.newValue OJournal
--                                        , tasksMax        = Value
--                                            $ S.newValue TasksMaxInfinity
--                                        }
--                                    , install = mempty
--                                                    { wantedBy =
--                                                        ["multi-user.target"]
--                                                    }
--                                    }
--                                )
--         H.it "network time service"
--             $ C.parser [r|[Unit]
-- Description=Network Time Service
-- After=network.target nss-lookup.target
-- Conflicts=systemd-timesyncd.service
--
-- [Service]
-- Type=forking
-- PrivateTmp=true
-- ExecStart=/usr/bin/ntpd -g -u ntp:ntp
-- Restart=always
--
-- [Install]
-- WantedBy=multi-user.target
-- |]
--             `HPP.shouldBe` C.ParseSuccess
--                                ((mempty :: SystemdService)
--                                    { unit    = mempty
--                                        { description = "Network Time Service"
--                                        , after       = [ "network.target"
--                                                        , "nss-lookup.target"
--                                                        ]
--                                        , conflicts   =
--
--                                            ["systemd-timesyncd.service"]
--                                        }
--                                    , service = mempty
--                                        { type_ = TForking
--                                            Empty
--                                            "/usr/bin/ntpd -g -u ntp:ntp"
--                                        , privateTmp =
--                                            Value $ S.newValue $ PrivateTmp True
--                                        , restart = Value $ S.newValue $ RAlways
--                                        }
--                                    , install = mempty
--                                                    { wantedBy =
--                                                        ["multi-user.target"]
--                                                    }
--                                    }
--                                )
--         H.it "networking for netctl profile %I"
--             $ C.parser [r|[Unit]
-- Description=Networking for netctl profile %I
-- Documentation=man:netctl.profile(5)
-- After=network-pre.target
-- Before=network.target netctl.service
-- Wants=network.target
--
-- [Service]
-- Type=notify
-- NotifyAccess=exec
-- RemainAfterExit=yes
-- ExecStart=/usr/lib/netctl/network start %I
-- ExecStop=/usr/lib/netctl/network stop %I
-- |]
--             `HPP.shouldBe` C.ParseSuccess
--                                ((mempty :: SystemdService)
--                                    { unit    = mempty
--                                        { description   =
--                                            "Networking for netctl profile %I"
--                                        , documentation =
--
--                                            ["man:netctl.profile(5)"]
--                                        , after         = ["network-pre.target"]
--                                        , before        = [ "network.target"
--                                                          , "netctl.service"
--                                                          ]
--                                        , wants         = ["network.target"]
--                                        }
--                                    , service = mempty
--                                        { type_           = TNotify
--                                            (S.newValue NAExec)
--                                            "/usr/lib/netctl/network start %I"
--                                        , remainAfterExit =
--                                            Value $ S.newValue $ RemainAfterExit
--                                                True
--                                        , execStop        =
--                                            ["/usr/lib/netctl/network stop %I"]
--                                        }
--                                    }
--                                )

--    H.describe "generate" $ do
--        H.it "with wrong path"
--            $ (C.generate $ C.fieldsTree [(C.Path ["hello"], "one")])
--            `HPP.shouldBe` ( [C.UnknownPath (C.Path ["hello"])]
--                           , Nothing :: Maybe SystemdService
--                           )
--        H.it "with description"
--            $              (C.generate $ C.fieldsTree
--                               [(C.Path ["Unit", "Description"], "my description")]
--                           )
--
--            `HPP.shouldBe` ( []
--                           , Just $ mempty
--                               { unit = mempty
--                                   { description = Value
--                                       $ Description "my description"
--                                   }
--                               }
--                           )
--        H.it "with description and after"
--            $              (C.generate $ C.fieldsTree
--                               [ (C.Path ["Unit", "Description"], "my description")
--                               , (C.Path ["Unit", "After"], "target1")
--                               , (C.Path ["Unit", "After"], "target2")
--                               ]
--                           )
--            `HPP.shouldBe` ( []
--                           , Just $ mempty
--                               { unit = mempty
--                                   { description = Value
--                                       $ Description "my description"
--                                   , after       = [ Target "target1"
--                                                   , Target "target2"
--                                                   ]
--                                   }
--                               }
--                           )
--        --H.it "with install"
--        --    $              (C.generate $ C.fieldsTree
--        --                       [ (C.Path ["Install", "WantedBy"], "me")
--        --                       , (C.Path ["Install", "RequiredBy"], "target1 target2")
--        --                       ]
--        --                   )
--        --    `HPP.shouldBe` ( []
--        --                   , Just $ mempty
--        --                       { install = mempty
--        --                                       { wantedBy   = [Target "me"]
--        --                                       , requiredBy = [ Target "target1"
--        --                                                      , Target "target2"
--        --                                                      ]
--        --                                       }
--        --                       }
--        --                   )
--        --H.it "with service"
--        --    $              (C.generate $ C.fieldsTree
--        --                       [ (C.Path ["Service", "User"]     , "me")
--        --                       , (C.Path ["Service", "ExecStart"], "cmd")
--        --                       ]
--        --                   )
--        --    `HPP.shouldBe` ( []
--        --                   , Just $ mempty
--        --                       { service = mempty
--        --                           { type_ = TSimple
--        --                                         (mempty { command = "cmd" })
--        --                           , user  = Value $ User "me"
--        --                           }
--        --                       }
--        --                   )
