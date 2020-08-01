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
import           Syntax.XDGDesktop              ( (<#) )
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
                                   { before = ResettableList
                                       ([ S.newValue $ ListElem $ Words
                                            ["one", "two"]
                                        , S.newValue $ ListElem $ Words
                                            ["three"]
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
                                       ([ S.newValue $ ListElem $ Words
                                            ["one", "two"]
                                        , S.newValue ListReset
                                        , S.newValue $ ListElem $ Words ["four"]
                                        ]
                                       )
                                   }
                               )
        H.it "with description"
            $              (C.parser :: S.Section -> C.ParseResult Unit)
                               (mempty /** ("Description", ["my service"]))
            `HPP.shouldBe` C.ParseSuccess
                               ((mempty :: Unit) { description = one
                                                     "my service"
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
                                           [ S.newValue $ ListElem $ Words
                                                 ["default.target"]
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
                                       [ S.newValue $ ListElem $ Words
                                           ["default.target"]
                                       , S.newValue $ ListElem $ Words
                                           ["1", "2", "3"]
                                       ]
                                   , requiredBy = ResettableList
                                       [ S.newValue $ ListElem $ Words
                                           ["other.target"]
                                       , S.newValue $ ListElem $ Words ["4"]
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
                                                   { description = one "my service"
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
                                                   { description = one "my service"
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
                                   , user             = one "aria2"
                                   , group            = one "aria2"
                                   , workingDirectory = one "/opt/AriaNg-1.1.1"
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
                                   , tasksMax = one $ S.newValue $ NotInfinity
                                                    10
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
                                   , tasksMax = one $ S.newValue $ Infinity
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
                                   , tasksMax = one
                                                $ S.setEnabled False
                                                $ S.newValue
                                                $ Infinity
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
                    { description = one "Aria2 Web Service"
                    , after       = ResettableList
                        [S.newValue $ ListElem $ Words ["network.target"]]
                    }
                , service = mempty
                    { user             = one "aria2"
                    , group            = one "aria2"
                    , workingDirectory = one "/opt/AriaNg-1.1.1"
                    , type_ = TSimple "/usr/bin/darkhttpd . --port 6810"
                    }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem $ Words ["default.target"]]
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
                    { description   = one "Laptop Mode Tools"
                    , documentation = List
                        [ S.newValue
                            $ Words
                                  [ DocMan "laptop_mode(8)"
                                  , DocMan "laptop-mode.conf(8)"
                                  ]
                        , S.newValue $ Words
                            [DocHTTP "github.com/rickysarraf/laptop-mode-tools"]
                        ]
                    }
                , service = mempty
                    { type_ = TOneShot ["/usr/bin/laptop_mode init force"]
                    , remainAfterExit = one $ S.newValue $ RemainAfterExit
                                            (setType BYes sTrue)
                    , execStartPre    = List
                        [ S.newValue
                            "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                        , S.newValue
                            "/bin/rm -f /var/run/laptop-mode-tools/state"
                        ]
                    , execStop        = List
                        [S.newValue "/usr/bin/laptop_mode init stop"]
                    , execStopPost    = List
                        [ S.newValue
                            "/bin/rm -f /var/run/laptop-mode-tools/enabled"
                        , S.newValue
                            "/bin/rm -f /var/run/laptop-mode-tools/state"
                        ]
                    , execReload = List [S.newValue "/usr/bin/laptop_mode auto"]
                    , standardOutput  = one $ S.newValue OJournal
                    , standardError   = one $ S.newValue OJournal
                    , tasksMax        = one $ S.newValue Infinity
                    }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem $ Words ["multi-user.target"]]
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
                    { description = one "Network Time Service"
                    , after       = ResettableList
                                        [ S.newValue $ ListElem $ Words
                                              ["network.target", "nss-lookup.target"]
                                        ]
                    , conflicts   =
                        ResettableList
                            [ S.newValue $ ListElem $ Words
                                  ["systemd-timesyncd.service"]
                            ]
                    }
                , service = mempty
                                { type_      = TForking
                                                   none
                                                   "/usr/bin/ntpd -g -u ntp:ntp"
                                , privateTmp = one
                                               $ S.newValue
                                               $ PrivateTmp
                                               $ setType BTrue sTrue
                                , restart    = one $ S.newValue $ RAlways
                                }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem $ Words ["multi-user.target"]]
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
                    { description   = one "Networking for netctl profile %I"
                    , documentation = List
                        [S.newValue $ Words [DocMan "netctl.profile(5)"]]
                    , after         = ResettableList
                        [S.newValue $ ListElem $ Words ["network-pre.target"]]
                    , before        = ResettableList
                                          [ S.newValue $ ListElem $ Words
                                                ["network.target", "netctl.service"]
                                          ]
                    , wants         = ResettableList
                        [S.newValue $ ListElem $ Words ["network.target"]]
                    }
                , service = mempty
                    { type_ = TNotify (one $ S.newValue NAExec)
                                      "/usr/lib/netctl/network start %I"
                    , remainAfterExit = one
                                        $ S.newValue
                                        $ RemainAfterExit
                                        $ setType BYes sTrue
                    , execStop = List
                        [S.newValue "/usr/lib/netctl/network stop %I"]
                    }
                }
            )
        fullRoundTrip
            "Network Manager"
            [r|[Unit]
Description=Network Manager
Documentation=man:NetworkManager(8)
Wants=network.target
After=network-pre.target dbus.service
Before=network.target

[Service]
Type=dbus
BusName=org.freedesktop.NetworkManager
ExecReload=/usr/bin/busctl call org.freedesktop.NetworkManager /org/freedesktop/NetworkManager org.freedesktop.NetworkManager Reload u 0
#ExecReload=/bin/kill -HUP $MAINPID
ExecStart=/usr/bin/NetworkManager --no-daemon
Restart=on-failure
# NM doesn't want systemd to kill its children for it
KillMode=process
CapabilityBoundingSet=CAP_NET_ADMIN CAP_DAC_OVERRIDE CAP_NET_RAW CAP_NET_BIND_SERVICE
CapabilityBoundingSet=~CAP_SETGID CAP_SETUID
CapabilityBoundingSet=CAP_SYS_MODULE CAP_AUDIT_WRITE CAP_KILL CAP_SYS_CHROOT
ProtectSystem=true
ProtectHome=read-only

[Install]
WantedBy=multi-user.target
Also=NetworkManager-dispatcher.service
# We want to enable NetworkManager-wait-online.service whenever this service
# is enabled. NetworkManager-wait-online.service has
# WantedBy=network-online.target, so enabling it only has an effect if
# network-online.target itself is enabled or pulled in by some other unit.
Also=NetworkManager-wait-online.service|]
            (mempty
                { unit    = mempty
                    { description   = one "Network Manager"
                    , documentation =
                        List $ [S.newValue $ Words [DocMan "NetworkManager(8)"]]
                    , wants         = ResettableList
                        [S.newValue $ ListElem $ Words ["network.target"]]
                    , after         = ResettableList
                                          [ S.newValue $ ListElem $ Words
                                                ["network-pre.target", "dbus.service"]
                                          ]
                    , before        = ResettableList
                        [S.newValue $ ListElem $ Words ["network.target"]]
                    }
                , service = mempty
                    { type_                 = TDBus
                        (S.newValue $ BusName "org.freedesktop.NetworkManager")
                        "/usr/bin/NetworkManager --no-daemon"
                    , execReload            = List
                        [ S.newValue
                            "/usr/bin/busctl call org.freedesktop.NetworkManager /org/freedesktop/NetworkManager org.freedesktop.NetworkManager Reload u 0"
                        , S.setEnabled False
                            $ S.newValue "/bin/kill -HUP $MAINPID"
                        ]
                    , restart               = one $ S.newValue ROnFailure
                    , killMode              =
                        one
                            $ (S.newValue KMProcess
                              <# " NM doesn't want systemd to kill its children for it"
                              )
                    , protectHome           = one $ S.newValue $ PMReadOnly
                    , protectSystem         = one $ S.newValue $ PSBool sTrue
                    , capabilityBoundingSet = ResettableList
                        [ S.newValue
                        $ ListElem
                        $ IncludedCapabilities
                        $ Words
                              [ CAP_NET_ADMIN
                              , CAP_DAC_OVERRIDE
                              , CAP_NET_RAW
                              , CAP_NET_BIND_SERVICE
                              ]
                        , S.newValue $ ListElem $ ExcludedCapabilities $ Words
                            [CAP_SETGID, CAP_SETUID]
                        , S.newValue
                        $ ListElem
                        $ IncludedCapabilities
                        $ Words
                              [ CAP_SYS_MODULE
                              , CAP_AUDIT_WRITE
                              , CAP_KILL
                              , CAP_SYS_CHROOT
                              ]
                        ]
                    }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem $ Words ["multi-user.target"]]
                    , also     = List
                        [ S.newValue
                            $ Words ["NetworkManager-dispatcher.service"]
                        , ( S.newValue
                          $ Words ["NetworkManager-wait-online.service"]
                          )
                            <# S.newComment
                                   "#"
                                   [ " We want to enable NetworkManager-wait-online.service whenever this service"
                                   , " is enabled. NetworkManager-wait-online.service has"
                                   , " WantedBy=network-online.target, so enabling it only has an effect if"
                                   , " network-online.target itself is enabled or pulled in by some other unit."
                                   ]
                        ]
                    }
                }
            )
        fullRoundTrip
            "Docker"
            [r|[Unit]
Description=Docker Application Container Engine
Documentation=https://docs.docker.com
After=network-online.target docker.socket firewalld.service
Wants=network-online.target
Requires=docker.socket

[Service]
Type=notify
# the default is not to use systemd for cgroups because the delegate issues still
# exists and systemd currently does not support the cgroup feature set required
# for containers run by docker
ExecStart=/usr/bin/dockerd -H fd://
ExecReload=/bin/kill -s HUP $MAINPID
LimitNOFILE=1048576
# Having non-zero Limit*s causes performance problems due to accounting overhead
# in the kernel. We recommend using cgroups to do container-local accounting.
LimitNPROC=infinity
LimitCORE=infinity
# Uncomment TasksMax if your systemd version supports it.
# Only systemd 226 and above support this version.
#TasksMax=infinity
TimeoutStartSec=0
# set delegate yes so that systemd does not reset the cgroups of docker containers
Delegate=yes
# kill only the docker process, not all processes in the cgroup
KillMode=process
# restart the docker process if it exits prematurely
Restart=on-failure
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target|]
            (mempty
                { unit    = mempty
                    { description   = one "Docker Application Container Engine"
                    , documentation =
                        List $ [S.newValue $ Words [DocHTTPS "docs.docker.com"]]
                    , wants         =
                        ResettableList
                            [ S.newValue $ ListElem $ Words
                                  ["network-online.target"]
                            ]
                    , after         = ResettableList
                        [ S.newValue
                          $ ListElem
                          $ Words
                                [ "network-online.target"
                                , "docker.socket"
                                , "firewalld.service"
                                ]
                        ]
                    , requires      = ResettableList
                        [S.newValue $ ListElem $ Words ["docker.socket"]]
                    }
                , service = mempty
                    { type_ = TNotify
                        none
                        (S.newValue "/usr/bin/dockerd -H fd://" <# S.newComment
                            "#"
                            [ " the default is not to use systemd for cgroups because the delegate issues still"
                            , " exists and systemd currently does not support the cgroup feature set required"
                            , " for containers run by docker"
                            ]
                        )
                    , execReload = List [S.newValue "/bin/kill -s HUP $MAINPID"]
                    , restart = one $ S.newValue ROnFailure <# S.newComment1
                        "#"
                        " restart the docker process if it exits prematurely"
                    , killMode =
                        one
                            $ (S.newValue KMProcess
                              <# " kill only the docker process, not all processes in the cgroup"
                              )
                    , tasksMax =
                        one
                        $  S.setEnabled False
                        $  S.newValue Infinity
                        <# S.newComment
                               "#"
                               [ " Uncomment TasksMax if your systemd version supports it."
                               , " Only systemd 226 and above support this version."
                               ]
                    , timeoutStartSec = one
                                        $ S.newValue
                                        $ NotInfinity
                                        $ Time
                                        $ [TimeNoUnit 0]
                    , delegate =
                        one $ S.newValue (setType BYes sTrue) <# S.newComment
                            "#"
                            [ " set delegate yes so that systemd does not reset the cgroups of docker containers"
                            ]
                    , limitNOFILE = one $ S.newValue $ LimitBoth $ NotInfinity
                                        1048576
                    , limitNPROC =
                        one
                        $  (S.newValue $ LimitBoth $ Infinity)
                        <# S.newComment
                               "#"
                               [ " Having non-zero Limit*s causes performance problems due to accounting overhead"
                               , " in the kernel. We recommend using cgroups to do container-local accounting."
                               ]
                    , limitCORE = one $ S.newValue $ LimitBoth $ Infinity
                    , startLimitBurst = one $ S.newValue 3
                    , startLimitInterval = one
                                           $ S.newValue
                                           $ Time
                                           $ [Seconds "s" 60]
                    }
                , install = mempty
                    { wantedBy = ResettableList
                        [S.newValue $ ListElem $ Words ["multi-user.target"]]
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
