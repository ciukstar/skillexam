{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoDataRU (populateRU) where

import ClassyPrelude.Yesod (ReaderT, forM_)

import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.ByteString as BS (readFile)
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime, addUTCTime )

import Database.Persist.Sql (SqlBackend, PersistStoreWrite (insert))
import Database.Persist ( PersistStoreWrite(insert_) )

import Model
    ( Skill(Skill, skillCode, skillName, skillDescr)
    , Candidate
      ( Candidate, candidateFamilyName, candidateGivenName, candidateAdditionalName
      , candidateBday, candidateUser, candidateEmail, candidatePhone
      )
    , Photo (Photo, photoCandidate, photoPhoto, photoMime)
    , Test
      ( Test, testCode, testName, testDuration, testDescr, testPass, testState
      , testDurationUnit
      )
    , TestState (TestStatePublished)
    , Stem (Stem, stemTest, stemSkill, stemOrdinal, stemText, stemType, stemInstruc)
    , Option (Option, optionStem, optionOrdinal, optionText, optionKey, optionPoints)
    , StemType (SingleRespose, MultiResponse)
    , Exam (Exam, examTest, examCandidate, examAttempt, examStart, examEnd, examStatus)
    , Answer (Answer, answerExam, answerStem, answerOption, answerTime)
    , User
      ( User, userEmail, userPassword, userName, userAdmin, userSuper, userAuthType
      , userVerkey, userVerified
      )
    , UserPhoto (UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    , AuthenticationType (UserAuthTypePassword), ExamStatus (ExamStatusCompleted)
    , TimeUnit (TimeUnitMinute), Social (Social, socialCandidate, socialLink)
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Form.Fields (Textarea (Textarea))
import Yesod.Auth.Email (saltPass)


populateRU :: MonadIO m => ReaderT SqlBackend m ()
populateRU = do
    (now,today) <- liftIO $ getCurrentTime >>= \x -> return (x,utctDay x)

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]    

    pass1 <- liftIO $ saltPass "ivivanov"
    let user1 = User { userEmail = "ivivanov@mail.ru"
                     , userPassword = Just pass1
                     , userName = Just "Игорь Васильевич Иванов"
                     , userSuper = False
                     , userAdmin = True
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid1 <- insert user1

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "lmbulanova"
    let user2 = User { userEmail = "lmbulanova@mail.ru"
                     , userPassword = Just pass2
                     , userName = Just "Любовь Михайловна Буланова"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid2 <- insert user2

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "iapetrov"
    let user3 = User { userEmail = "iapetrov@mail.ru"
                     , userPassword = Just pass3
                     , userName = Just "Иван Александрович Петров"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid3 <- insert user3

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "mvlebedeva"
    let user4 = User { userEmail = "mvlebedeva@mail.ru"
                     , userPassword = Just pass4
                     , userName = Just "Марина Викторовна Лебедева"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid4 <- insert user4

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    c001 <- insert $ Candidate
               { candidateFamilyName = "Иванов"
               , candidateGivenName = "Игорь"
               , candidateAdditionalName = Just "Васильевич"
               , candidateBday = Just $ addGregorianYearsClip (-28) today
               , candidateEmail = Just (userEmail user1)
               , candidatePhone = Just "+7 (958) 759-52-25"
               , candidateUser = Just uid1
               }
    insert_ Social { socialCandidate = c001
                   , socialLink = "https://wa.me/79587595225"
                   }
    insert_ Social { socialCandidate = c001
                   , socialLink = "https://t.me/+79587595225"
                   }
    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c001
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c002 <- insert $ Candidate
               { candidateFamilyName = "Буланова"
               , candidateGivenName = "Любовь"
               , candidateAdditionalName = Just "Михайловна"
               , candidateBday = Just $ addGregorianYearsClip (-26) today
               , candidateEmail = Just (userEmail user2)
               , candidatePhone = Just "+7 (958) 759-52-26"
               , candidateUser = Just uid2
               }
    insert_ Social { socialCandidate = c002
                   , socialLink = "https://wa.me/79587595226"
                   }
    insert_ Social { socialCandidate = c002
                   , socialLink = "https://t.me/+79587595226"
                   } 
    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c002
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c003 <- insert $ Candidate
               { candidateFamilyName = "Петров"
               , candidateGivenName = "Иван"
               , candidateAdditionalName = Just "Александрович"
               , candidateBday = Just $ addGregorianYearsClip (-21) today
               , candidateEmail = Just (userEmail user3)
               , candidatePhone = Just "+7 (958) 759-52-27"
               , candidateUser = Just uid3
               }
    insert_ Social { socialCandidate = c003
                   , socialLink = "https://wa.me/79587595227"
                   }
    insert_ Social { socialCandidate = c003
                   , socialLink = "https://t.me/+79587595227"
                   } 
    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c003
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c004 <- insert $ Candidate
               { candidateFamilyName = "Лебедева"
               , candidateGivenName = "Марина"
               , candidateAdditionalName = Just "Викторовна"
               , candidateBday = Just $ addGregorianYearsClip (-30) today
               , candidateEmail = Just (userEmail user4)
               , candidatePhone = Just "+7 (958) 759-52-28"
               , candidateUser = Just uid4
               }
    insert_ Social { socialCandidate = c004
                   , socialLink = "https://wa.me/79587595228"
                   }
    insert_ Social { socialCandidate = c004
                   , socialLink = "https://t.me/+79587595228"
                   }
    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c004
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c005 <- insert $ Candidate
               { candidateFamilyName = "Смирнов"
               , candidateGivenName = "Андрей"
               , candidateAdditionalName = Just "Васильевич"
               , candidateBday = Just $ addGregorianYearsClip (-32) today
               , candidateEmail = Just "asmirnov@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-29"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c005
                   , socialLink = "https://wa.me/79587595229"
                   }
    insert_ Social { socialCandidate = c005
                   , socialLink = "https://t.me/+79587595229"
                   }
    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c005
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c006 <- insert $ Candidate
               { candidateFamilyName = "Иванов"
               , candidateGivenName = "Алексей"
               , candidateAdditionalName = Just "Васильевич"
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               , candidateEmail = Just "aivanov@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-30"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c006
                   , socialLink = "https://wa.me/79587595230"
                   }
    insert_ Social { socialCandidate = c006
                   , socialLink = "https://t.me/+79587595230"
                   }
    liftIO (BS.readFile "demo/user_7.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c006
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c007 <- insert $ Candidate
               { candidateFamilyName = "Сергеева"
               , candidateGivenName = "Александра"
               , candidateAdditionalName = Just "Владимировна"
               , candidateBday = Just $ addGregorianYearsClip (-35) today
               , candidateEmail = Just "asergeeva@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-31"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c007
                   , socialLink = "https://wa.me/79587595231"
                   }
    insert_ Social { socialCandidate = c007
                   , socialLink = "https://t.me/+79587595231"
                   }
    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c007
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c008 <- insert $ Candidate
               { candidateFamilyName = "Степанова"
               , candidateGivenName = "Татьяна"
               , candidateAdditionalName = Just "Николаевна"
               , candidateBday = Just $ addGregorianYearsClip (-42) today
               , candidateEmail = Just "tstepanova@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-32"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c008
                   , socialLink = "https://wa.me/79587595232"
                   }
    insert_ Social { socialCandidate = c008
                   , socialLink = "https://t.me/+79587595232"
                   }
    liftIO (BS.readFile "demo/user_8.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c008
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c009 <- insert $ Candidate
               { candidateFamilyName = "Кузнецов"
               , candidateGivenName = "Артем"
               , candidateAdditionalName = Just "Сергеевич"
               , candidateBday = Just $ addGregorianYearsClip (-46) today
               , candidateEmail = Just "akuznetsov@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-33"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c009
                   , socialLink = "https://wa.me/79587595233"
                   }
    insert_ Social { socialCandidate = c009
                   , socialLink = "https://t.me/+79587595233"
                   }
    liftIO (BS.readFile "demo/user_9.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c009
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c010 <- insert $ Candidate
               { candidateFamilyName = "Попов"
               , candidateGivenName = "Дмитрий"
               , candidateAdditionalName = Just "Александрович"
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               , candidateEmail = Just "dpopov@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-34"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c010
                   , socialLink = "https://wa.me/79587595234"
                   }
    insert_ Social { socialCandidate = c010
                   , socialLink = "https://t.me/+79587595234"
                   }
    liftIO (BS.readFile "demo/user_10.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c010
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c011 <- insert $ Candidate
               { candidateFamilyName = "Баранова"
               , candidateGivenName = "Алиса"
               , candidateAdditionalName = Just "Григорьевна"
               , candidateBday = Just $ addGregorianYearsClip (-31) today
               , candidateEmail = Just "abaranova@mail.ru"
               , candidatePhone = Just "+7 (958) 759-52-35"
               , candidateUser = Nothing
               }
    insert_ Social { socialCandidate = c011
                   , socialLink = "https://wa.me/79587595235"
                   }
    insert_ Social { socialCandidate = c011
                   , socialLink = "https://t.me/+79587595235"
                   }
    liftIO (BS.readFile "demo/user_11.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c011
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    s101 <- insert $ Skill { skillCode = "JavaSE"
                    , skillName = "Стандартная версия Java"
                    , skillDescr = Just "Навыки программирования на JavaSE"
                    }

    e101 <- insert $ Test
        { testCode = "Э101"
        , testName = "Основы программирования на Java"
        , testDuration = 20
        , testDurationUnit = TimeUnitMinute
        , testPass = 10
        , testDescr = Just $ Textarea "Тестирование кандидатов на базовые навыки программирования на Java."
        , testState = TestStatePublished
        }

    q101 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 1
               , stemText = Textarea "Какой из следующих вариантов обеспечивает переносимость и безопасность Java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k101 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "а)"
        , optionText = Textarea "Байт-код выполняется JVM"
        , optionKey = True
        , optionPoints = 1
        }

    d101_1 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "б)"
        , optionText = Textarea "Апплет делает код Java безопасным и переносимым"
        , optionKey = False
        , optionPoints = 0
        }

    d101_2 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "в)"
        , optionText = Textarea "Использование обработки исключений"
        , optionKey = False
        , optionPoints = 0
        }

    d101_3 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "г)"
        , optionText = Textarea "Динамическая привязка между объектами"
        , optionKey = False
        , optionPoints = 0
        }

    q102 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 2
               , stemText = Textarea "Что из перечисленного не относится к функциям Java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d102_1 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "а)"
        , optionText = Textarea "Динамический"
        , optionKey = False
        , optionPoints = 0
        }

    d102_2 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "б)"
        , optionText = Textarea "Нейтральная архитектура"
        , optionKey = False
        , optionPoints = 0
        }

    k102 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "в)"
        , optionText = Textarea "Использование указателей"
        , optionKey = True
        , optionPoints = 1
        }

    d102_3 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "г)"
        , optionText = Textarea "Объектно-ориентированный"
        , optionKey = False
        , optionPoints = 0
        }

    q103 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 3
               , stemText = Textarea [st|Каким должен быть порядок выполнения, если класс имеет метод, статический блок, блок экземпляра и конструктор, как показано ниже?
<code>
<pre>
public class First_C {
  public void myMethod() {
    System.out.println("Method");
  }

  {
    System.out.println(" Instance Block");
  }

  public void First_C() {
    System.out.println("Constructor ");
  }
  static {
    System.out.println("static block");
  }
  public static void main(String[] args) {
    First_C c = new First_C();
    c.First_C();
    c.myMethod();
  }
}
</pre>
</code>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d103_1 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "а)"
        , optionText = Textarea "Блок экземпляра, метод, статический блок и конструктор"
        , optionKey = False
        , optionPoints = 0
        }

    d103_2 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "б)"
        , optionText = Textarea "Метод, конструктор, блок экземпляра и статический блок"
        , optionKey = False
        , optionPoints = 0
        }

    d103_3 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "в)"
        , optionText = Textarea "Статический блок, метод, блок экземпляра и конструктор"
        , optionKey = False
        , optionPoints = 0
        }

    k103 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "г)"
        , optionText = Textarea "Статический блок, блок экземпляра, конструктор и метод"
        , optionKey = True
        , optionPoints = 1
        }

    q104 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 4
               , stemText = Textarea [st|Что выведет следующая программа?
<code><pre>
public class MyFirst {
  public static void main(String[] args) {
    MyFirst obj = new MyFirst(n);
  }
  static int a = 10;
  static int n;
  int b = 5;
  int c;
  public MyFirst(int m) {
    System.out.println(
      a + ", " + b + ", " + c + ", " + n + ", " + m
    );
  }
  // Экземплярный блок
  {
    b = 30;
    n = 20;
  }
  // Статический блок
  static
  {
    a = 60;
  }
}</pre></code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d104_1 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "а)"
        , optionText = Textarea "10, 5, 0, 20, 0"
        , optionKey = False
        , optionPoints = 0
        }

    d104_2 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "б)"
        , optionText = Textarea "10, 30, 20"
        , optionKey = False
        , optionPoints = 0
        }

    d104_3 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "в)"
        , optionText = Textarea "60, 5, 0, 20"
        , optionKey = False
        , optionPoints = 0
        }

    k104 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "г)"
        , optionText = Textarea "60, 30, 0, 20, 0"
        , optionKey = True
        , optionPoints = 1
        }

    q105 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 5
               , stemText = Textarea "Статья <code>\\u0021</code>, именуемая"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k105 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "а)"
        , optionText = Textarea "Управляющая последовательность Unicode"
        , optionKey = True
        , optionPoints = 1
        }

    d105_1 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "б)"
        , optionText = Textarea "Восьмеричный побег"
        , optionKey = False
        , optionPoints = 0
        }

    d105_2 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "в)"
        , optionText = Textarea "шестнадцатеричный"
        , optionKey = False
        , optionPoints = 0
        }

    d105_3 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "г)"
        , optionText = Textarea "Перевод строки"
        , optionKey = False
        , optionPoints = 0
        }

    q106 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 6
               , stemText = Textarea " _____ используется для поиска и исправления ошибок в программах Java"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d106_1 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "а)"
        , optionText = Textarea "JVM"
        , optionKey = False
        , optionPoints = 0
        }

    d106_2 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "б)"
        , optionText = Textarea "JRE"
        , optionKey = False
        , optionPoints = 0
        }

    d106_3 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "в)"
        , optionText = Textarea "JDK"
        , optionKey = False
        , optionPoints = 0
        }

    k106 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "г)"
        , optionText = Textarea "JDB"
        , optionKey = True
        , optionPoints = 1
        }

    q107 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 7
               , stemText = Textarea "Что из следующего является допустимым объявлением <code>char</code>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k107 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "а)"
        , optionText = Textarea "<code>char ch = '\\utea';</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d107_1 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "б)"
        , optionText = Textarea "<code>char ca = 'tea';</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d107_2 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "в)"
        , optionText = Textarea "<code>char cr = \\u0223;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d107_3 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "г)"
        , optionText = Textarea "<code>char cc = '\\itea';</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q108 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 8
               , stemText = Textarea "Какой тип возвращает метод <code>hashCode()</code> в классе <code>Object</code>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d108_1 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "а)"
        , optionText = Textarea "<code>Object</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k108 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "б)"
        , optionText = Textarea "<code>int</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d108_2 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "в)"
        , optionText = Textarea "<code>long</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d108_3 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "г)"
        , optionText = Textarea "<code>void</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q109 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 9
               , stemText = Textarea "Что из следующего является допустимым длинным литералом?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d109_1 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "а)"
        , optionText = Textarea "<code>ABH8097</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d109_2 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "б)"
        , optionText = Textarea "<code>L990023</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d109_3 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "в)"
        , optionText = Textarea "<code>904423</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k109 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "г)"
        , optionText = Textarea "<code>0xnf029L</code>"
        , optionKey = True
        , optionPoints = 1
        }

    q110 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 10
               , stemText = Textarea "Что возвращает выражение <code>float a = 35 / 0</code>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d110_1 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "а)"
        , optionText = Textarea "<code>0</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d110_2 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "б)"
        , optionText = Textarea "Not a Number"
        , optionKey = False
        , optionPoints = 0
        }

    k110 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "в)"
        , optionText = Textarea "<code>Infinity</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d110_3 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "г)"
        , optionText = Textarea "Исключение времени выполнения"
        , optionKey = False
        , optionPoints = 0
        }

    q111 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 11
               , stemText = Textarea "Оцените следующее выражение Java, если x=3, y=5 и z=10:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d111_1 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "а)"
        , optionText = Textarea "24"
        , optionKey = False
        , optionPoints = 0
        }

    d111_2 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "б)"
        , optionText = Textarea "23"
        , optionKey = False
        , optionPoints = 0
        }

    d111_3 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "в)"
        , optionText = Textarea "20"
        , optionKey = False
        , optionPoints = 0
        }

    k111 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "г)"
        , optionText = Textarea "25"
        , optionKey = True
        , optionPoints = 1
        }

    q112 <- insert $ Stem
               { stemTest = e101
               , stemSkill = s101
               , stemOrdinal = 12
               , stemText = Textarea [st|"Что выведет следующая программа?"
<code><pre>
public class Test {
  public static void main(String[] args) {
    int count = 1;
    while (count <= 15) {
      System.out.println(count % 2 == 1 ? "***" : "+++++");
      ++count;
    } // end while
  } // end main
}</pre></code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d112_1 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "а)"
        , optionText = Textarea "15 раз ***"
        , optionKey = False
        , optionPoints = 0
        }

    d112_2 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "б)"
        , optionText = Textarea "15 раз +++++"
        , optionKey = False
        , optionPoints = 0
        }

    k112 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "в)"
        , optionText = Textarea "8 раз *** и 7 раз +++++"
        , optionKey = True
        , optionPoints = 1
        }

    d112_3 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "г)"
        , optionText = Textarea "Оба будут печатать только один раз"
        , optionKey = False
        , optionPoints = 0
        }

    r101 <- insert $ Exam
        { examTest = e101
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-1112) now
        , examEnd = pure $ addUTCTime (-1100) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r101
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r102 <- insert $ Exam
        { examTest = e101
        , examCandidate = c002
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-2112) now
        , examEnd = pure $ addUTCTime (-2100) now
        }

    forM_ [ (q101,k101)
          , (q102,d102_1)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,d106_2)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r102
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r103 <- insert $ Exam
        { examTest = e101
        , examCandidate = c003
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-3112) now
        , examEnd = pure $ addUTCTime (-3101) now
        }

    forM_ [ (q101,k101)
          , (q102,d102_1)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,d106_2)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_3)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r103
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r104 <- insert $ Exam
        { examTest = e101
        , examCandidate = c004
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4113) now
        , examEnd = pure $ addUTCTime (-3100) now
        }

    forM_ [ (q101,k101)
          , (q102,d102_1)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_3)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r104
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r105 <- insert $ Exam
        { examTest = e101
        , examCandidate = c005
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5024) now
        , examEnd = pure $ addUTCTime (-5005) now
        }

    forM_ [ (q101,k101)
          , (q102,d102_1)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r105
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r106 <- insert $ Exam
        { examTest = e101
        , examCandidate = c006
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-6024) now
        , examEnd = pure $ addUTCTime (-6006) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r106
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r107 <- insert $ Exam
        { examTest = e101
        , examCandidate = c007
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5026) now
        , examEnd = pure $ addUTCTime (-5008) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,d103_1)
          , (q104,k104)
          , (q105,d105_2)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_3)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r107
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r108 <- insert $ Exam
        { examTest = e101
        , examCandidate = c008
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7026) now
        , examEnd = pure $ addUTCTime (-7008) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,d108_2)
          , (q109,k109)
          , (q110,d110_3)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r108
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r109 <- insert $ Exam
        { examTest = e101
        , examCandidate = c009
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-8020) now
        , examEnd = pure $ addUTCTime (-8000) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,d107_2)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,d112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r109
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r110 <- insert $ Exam
        { examTest = e101
        , examCandidate = c010
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4020) now
        , examEnd = pure $ addUTCTime (-4001) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,d104_3)
          , (q105,k105)
          , (q106,k106)
          , (q107,d107_2)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,d112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r110
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r111 <- insert $ Exam
        { examTest = e101
        , examCandidate = c011
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4120) now
        , examEnd = pure $ addUTCTime (-4101) now
        }

    forM_ [ (q101,k101)
          , (q102,k102)
          , (q103,k103)
          , (q104,d104_3)
          , (q105,k105)
          , (q106,k106)
          , (q107,d107_2)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r111
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s201 <- insert $ Skill
        { skillCode = "СУБД"
        , skillName = "Управления базами данных"
        , skillDescr = Just "Навыки управления базами данных"
        }

    e201 <- insert $ Test
        { testCode = "Э201"
        , testName = "Системы управления базами данных"
        , testDuration = 30
        , testDurationUnit = TimeUnitMinute
        , testPass = 9
        , testDescr = Just $ Textarea "Экзамен по системам управления базами данных"
        , testState = TestStatePublished
        }

    q201 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 1
               , stemText = Textarea "Что из следующего обычно используется для выполнения таких задач, как создание структуры отношений, удаление отношения?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d201_1 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "а."
        , optionText = Textarea "DML (язык манипулирования данными)"
        , optionKey = False
        , optionPoints = 0
        }

    d201_2 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "б."
        , optionText = Textarea "Запрос"
        , optionKey = False
        , optionPoints = 0
        }

    d201_3 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "в."
        , optionText = Textarea "Реляционная схема"
        , optionKey = False
        , optionPoints = 0
        }

    k201 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "г."
        , optionText = Textarea "DDL (язык определения данных)"
        , optionKey = True
        , optionPoints = 1
        }

    q202 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 2
               , stemText = Textarea "Что из следующего дает возможность запрашивать информацию из базы данных и вставлять кортежи, удалять кортежи и изменять кортежи в базе данных?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k202 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "а."
        , optionText = Textarea "DML (язык манипулирования данными)"
        , optionKey = True
        , optionPoints = 1
        }

    d202_1 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "б."
        , optionText = Textarea "DDL (язык определения данных)"
        , optionKey = False
        , optionPoints = 0
        }

    d202_2 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "в."
        , optionText = Textarea "Запрос"
        , optionKey = False
        , optionPoints = 0
        }

    d202_3 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "г."
        , optionText = Textarea "Реляционная схема"
        , optionKey = False
        , optionPoints = 0
        }

    q203 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 3
               , stemText = Textarea [st|Данный запрос также можно заменить на_______:
<code><pre>
SELECT name, course_id
FROM instructor, teaches
WHERE instructor_ID= teaches_ID;
</pre><code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d203_1 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "а."
        , optionText = Textarea "<code>Select name,course_id from teaches,instructor where instructor_id=course_id;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k203 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "б."
        , optionText = Textarea "<code>Select name, course_id from instructor natural join teaches;</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d203_2 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "в."
        , optionText = Textarea "<code>Select name, course_id from instructor;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d203_3 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "г."
        , optionText = Textarea "<code>Select course_id from instructor join teaches;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q204 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 4
               , stemText = Textarea "Какое из следующих приведенных утверждений, возможно, содержит ошибку?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d204_1 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "а."
        , optionText = Textarea "<code>select * from emp where empid = 10003;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d204_2 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "б."
        , optionText = Textarea "<code>select empid from emp where empid = 10006;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d204_3 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "в."
        , optionText = Textarea "<code>select empid from emp;</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k204 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "г."
        , optionText = Textarea "<code>select empid where empid = 1009 and Lastname = 'GELLER';</code>"
        , optionKey = True
        , optionPoints = 1
        }

    q205 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 5
               , stemText = Textarea [st|Внимательно подготовьте запрос:
<code><pre>
SELECT emp_name
FROM department
WHERE dept_name LIKE ' _____ Computer Science';
</pre><code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d205_1 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "а."
        , optionText = Textarea "<code>&</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d205_2 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "б."
        , optionText = Textarea "<code>_</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k205 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "в."
        , optionText = Textarea "<code>%</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d205_3 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "г."
        , optionText = Textarea "<code>$</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q206 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 6
               , stemText = Textarea [st|Что вы подразумеваете под отношениями один ко многим?|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d206_1 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "а."
        , optionText = Textarea "В одном классе может быть много учителей"
        , optionKey = False
        , optionPoints = 0
        }

    k206 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "б."
        , optionText = Textarea "У одного учителя может быть много классов"
        , optionKey = True
        , optionPoints = 1
        }

    d206_2 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "в."
        , optionText = Textarea "Во многих классах может быть много учителей"
        , optionKey = False
        , optionPoints = 0
        }

    d206_3 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "г."
        , optionText = Textarea "У многих учителей может быть много классов"
        , optionKey = False
        , optionPoints = 0
        }

    q207 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 7
               , stemText = Textarea [st|В следующем запросе, что из следующего можно поместить в пустую часть запроса, чтобы отобразить зарплату от самой высокой до самой низкой суммы и отсортировать имена сотрудников в алфавитном порядке?
<code><pre>
SELECT *
FROM instructor
ORDER BY salary ____, name ___;
</pre></code>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d207_1 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "а."
        , optionText = Textarea "<code>Ascending, Descending</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d207_2 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "б."
        , optionText = Textarea "<code>Asc, Desc</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k207 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "в."
        , optionText = Textarea "Desc, Asc"
        , optionKey = True
        , optionPoints = 1
        }

    d207_3 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "г."
        , optionText = Textarea "Все вышеперечисленное"
        , optionKey = False
        , optionPoints = 0
        }

    q208 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 8
               , stemText = Textarea [st|Данный Запрос можно заменить на ____________:
<code><pre>
SELECT name
FROM instructor1
WHERE salary <= 100000 AND salary >= 90000;
</pre></code>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d208_1 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "а."
        , optionText = Textarea [st|<code><pre>SELECT name
FROM instructor1
WHERE salary BETWEEN 100000 AND 90000</pre></code>|]
        , optionKey = False
        , optionPoints = 0
        }

    d208_2 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "б."
        , optionText = Textarea [st|<code><pre>SELECT name
FROM instructor1
WHERE salary BETWEEN 90000 AND 100000;</pre></code>|]
        , optionKey = False
        , optionPoints = 0
        }

    k208 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "в."
        , optionText = Textarea [st|<code><pre>SELECT name
FROM instructor1
WHERE salary BETWEEN 90000 AND 100000;</pre></code>|]
        , optionKey = True
        , optionPoints = 1
        }

    d208_3 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "г."
        , optionText = Textarea [st|<code><pre>SELECT name
FROM instructor!
WHERE salary <= 90000 AND salary>=100000;</pre></code>|]
        , optionKey = False
        , optionPoints = 0
        }

    q209 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 9
               , stemText = Textarea [st|Система управления базами данных — это тип _________ программного обеспечения|]
               , stemType = MultiResponse
               , stemInstruc = Textarea "Выберите все правильные"
               }

    k209 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "а."
        , optionText = Textarea [st|Это тип системного программного обеспечения|]
        , optionKey = True
        , optionPoints = 1
        }

    d209_1 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "б."
        , optionText = Textarea [st|Это своего рода прикладное программное обеспечение|]
        , optionKey = False
        , optionPoints = 0
        }

    d209_2 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "в."
        , optionText = Textarea [st|Это своего рода общее программное обеспечение|]
        , optionKey = False
        , optionPoints = 0
        }

    d209_3 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "г."
        , optionText = Textarea [st|Файловая система|]
        , optionKey = False
        , optionPoints = 0
        }

    q210 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 10
               , stemText = Textarea [st|Термин "FAT" означает_____|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d210_1 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "а."
        , optionText = Textarea [st|Дерево размещения файлов|]
        , optionKey = False
        , optionPoints = 0
        }

    k210 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "б."
        , optionText = Textarea [st|Таблица размещения файлов|]
        , optionKey = True
        , optionPoints = 1
        }

    d210_2 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "в."
        , optionText = Textarea [st|График размещения файлов|]
        , optionKey = False
        , optionPoints = 0
        }

    d210_3 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "г."
        , optionText = Textarea [st|Все вышеперечисленное|]
        , optionKey = False
        , optionPoints = 0
        }

    q211 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 11
               , stemText = Textarea [st|Что из следующего можно считать максимальным размером, поддерживаемым FAT?|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d211_1 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "а."
        , optionText = Textarea [st|8 ГБ|]
        , optionKey = False
        , optionPoints = 0
        }

    k211 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "б."
        , optionText = Textarea [st|4ГБ|]
        , optionKey = True
        , optionPoints = 1
        }

    d211_2 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "в."
        , optionText = Textarea [st|4 ТБ|]
        , optionKey = False
        , optionPoints = 0
        }

    d211_3 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "г."
        , optionText = Textarea [st|Ни один из вышеперечисленных|]
        , optionKey = False
        , optionPoints = 0
        }

    q212 <- insert $ Stem
               { stemTest = e201
               , stemSkill = s201
               , stemOrdinal = 12
               , stemText = Textarea [st|Термин «NTFS» относится к чему из следующего?|]
               , stemType = MultiResponse
               , stemInstruc = Textarea "Выберите все правильные"
               }

    k212 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "а."
        , optionText = Textarea [st|Файловая система новой технологии|]
        , optionKey = True
        , optionPoints = 1
        }

    d212_1 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "б."
        , optionText = Textarea [st|Новая файловая система дерева|]
        , optionKey = False
        , optionPoints = 0
        }

    d212_2 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "в."
        , optionText = Textarea [st|Новая файловая система табличного типа|]
        , optionKey = False
        , optionPoints = 0
        }

    d212_3 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "г."
        , optionText = Textarea [st|Нетабличная файловая система|]
        , optionKey = False
        , optionPoints = 0
        }

    r201 <- insert $ Exam
        { examTest = e201
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4120) now
        , examEnd = pure $ addUTCTime (-4101) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,d207_2)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r201
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r202 <- insert $ Exam
        { examTest = e201
        , examCandidate = c002
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4124) now
        , examEnd = pure $ addUTCTime (-4105) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,d205_2)
          , (q206,k206)
          , (q207,d207_2)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r202
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r203 <- insert $ Exam
        { examTest = e201
        , examCandidate = c003
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5124) now
        , examEnd = pure $ addUTCTime (-5106) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,d211_1)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r203
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r204 <- insert $ Exam
        { examTest = e201
        , examCandidate = c004
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-6124) now
        , examEnd = pure $ addUTCTime (-6106) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,d211_1)
          , (q212,d212_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r204
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r205 <- insert $ Exam
        { examTest = e201
        , examCandidate = c005
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5024) now
        , examEnd = pure $ addUTCTime (-5007) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,d211_1)
          , (q212,d212_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r205
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r206 <- insert $ Exam
        { examTest = e201
        , examCandidate = c006
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5624) now
        , examEnd = pure $ addUTCTime (-5607) now
        }

    forM_ [ (q201,d201_2)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,k211)
          , (q212,d212_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r206
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r207 <- insert $ Exam
        { examTest = e201
        , examCandidate = c007
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5724) now
        , examEnd = pure $ addUTCTime (-5708) now
        }

    forM_ [ (q201,d201_2)
          , (q202,k202)
          , (q203,k203)
          , (q204,d204_3)
          , (q205,k205)
          , (q206,k206)
          , (q207,d207_1)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,k211)
          , (q212,d212_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r207
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r208 <- insert $ Exam
        { examTest = e201
        , examCandidate = c008
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5824) now
        , examEnd = pure $ addUTCTime (-5808) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,k204)
          , (q205,k205)
          , (q206,d206_3)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,d212_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r208
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r209 <- insert $ Exam
        { examTest = e201
        , examCandidate = c009
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-6824) now
        , examEnd = pure $ addUTCTime (-6808) now
        }

    forM_ [ (q201,k201)
          , (q202,d202_1)
          , (q203,k203)
          , (q204,k204)
          , (q205,k205)
          , (q206,d206_3)
          , (q207,k207)
          , (q208,d208_2)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r209
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r210 <- insert $ Exam
        { examTest = e201
        , examCandidate = c010
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7824) now
        , examEnd = pure $ addUTCTime (-7808) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,d203_3)
          , (q204,k204)
          , (q205,k205)
          , (q206,d206_3)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r210
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r211 <- insert $ Exam
        { examTest = e201
        , examCandidate = c011
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7924) now
        , examEnd = pure $ addUTCTime (-7908) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,d203_3)
          , (q204,k204)
          , (q205,k205)
          , (q206,k206)
          , (q207,d207_1)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r211
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s301 <- insert $ Skill
        { skillCode = "Маркетинг"
        , skillName = "Маркетинг"
        , skillDescr = Just "Маркетинговые навыки"
        }

    e301 <- insert $ Test
        { testCode = "Э301"
        , testName = "Маркетинг"
        , testDuration = 30
        , testDurationUnit = TimeUnitMinute
        , testPass = 12
        , testDescr = Just $ Textarea "Экзамен по маркетингу"
        , testState = TestStatePublished
        }

    q301 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 1
               , stemText = Textarea "Четыре уникальных элемента услуг включают в себя:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d301_1 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "А."
        , optionText = Textarea "Независимость, неосязаемость, инвентарь и начало"
        , optionKey = False
        , optionPoints = 0
        }

    d301_2 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "Б."
        , optionText = Textarea "Независимость, увеличение, запасы и неосязаемость"
        , optionKey = False
        , optionPoints = 0
        }

    k301 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "В."
        , optionText = Textarea "Неосязаемость, непоследовательность, неотделимость и запасы"
        , optionKey = True
        , optionPoints = 1
        }

    d301_3 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "Г."
        , optionText = Textarea "Неосязаемость, независимость, неделимость и запасы"
        , optionKey = False
        , optionPoints = 0
        }

    q302 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 2
               , stemText = Textarea "К какому из следующих понятий относится изменение поведения человека, вызванное информацией и опытом?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k302 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "А."
        , optionText = Textarea "Обучение"
        , optionKey = True
        , optionPoints = 1
        }

    d302_1 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "Б."
        , optionText = Textarea "Выбор роли"
        , optionKey = False
        , optionPoints = 0
        }

    d302_2 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "В."
        , optionText = Textarea "Восприятие"
        , optionKey = False
        , optionPoints = 0
        }

    d302_3 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "Г."
        , optionText = Textarea "Мотивация"
        , optionKey = False
        , optionPoints = 0
        }

    q303 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 3
               , stemText = Textarea "При покупке молока какое поведение демонстрирует человек?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d303_1 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "А."
        , optionText = Textarea "Активное решение проблем"
        , optionKey = False
        , optionPoints = 0
        }

    k303 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "Б."
        , optionText = Textarea "Рутинное покупательское поведение"
        , optionKey = True
        , optionPoints = 1
        }

    d303_2 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "В."
        , optionText = Textarea "Поведение, стремящееся к разнообразию"
        , optionKey = False
        , optionPoints = 0
        }

    d303_3 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "Г."
        , optionText = Textarea "Ни один из вышеперечисленных"
        , optionKey = False
        , optionPoints = 0
        }

    q304 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 4
               , stemText = Textarea "Продавать ли через посредников или напрямую потребителям, через сколько торговых точек продавать, контролировать или сотрудничать с другими участниками канала — вот примеры решений, которые должны принимать маркетологи."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d304_1 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "А."
        , optionText = Textarea "Повышение"
        , optionKey = False
        , optionPoints = 0
        }

    d304_2 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "Б."
        , optionText = Textarea "Цена"
        , optionKey = False
        , optionPoints = 0
        }

    k304 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "В."
        , optionText = Textarea "Распределение"
        , optionKey = True
        , optionPoints = 1
        }

    d304_3 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "Г."
        , optionText = Textarea "Продукт"
        , optionKey = False
        , optionPoints = 0
        }

    q305 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 5
               , stemText = Textarea "Расширенный маркетинговый комплекс услуг Ps:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d305_1 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "А."
        , optionText = Textarea "Люди, Продукт, Место"
        , optionKey = False
        , optionPoints = 0
        }

    d305_2 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "Б."
        , optionText = Textarea "Цена Вещественные доказательства, продвижение"
        , optionKey = False
        , optionPoints = 0
        }

    k305 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "В."
        , optionText = Textarea "Вещественные доказательства, Процесс, Люди"
        , optionKey = True
        , optionPoints = 1
        }

    d305_3 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "Г."
        , optionText = Textarea "Продукт, процесс, физическая среда"
        , optionKey = False
        , optionPoints = 0
        }

    q306 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 6
               , stemText = Textarea "К какому из следующих понятий относится социальный и управленческий процесс, с помощью которого отдельные лица и организации получают то, что им нужно и чего они хотят, путем создания ценности?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d306_1 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "А."
        , optionText = Textarea "Продажа"
        , optionKey = False
        , optionPoints = 0
        }

    d306_2 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "Б."
        , optionText = Textarea "Реклама"
        , optionKey = False
        , optionPoints = 0
        }

    d306_3 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "В."
        , optionText = Textarea "Бартер"
        , optionKey = False
        , optionPoints = 0
        }

    k306 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "Г."
        , optionText = Textarea "Маркетинг"
        , optionKey = True
        , optionPoints = 1
        }

    q307 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 7
               , stemText = Textarea "Какое основное свойство услуги отличает ее от продукта?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d307_1 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "А."
        , optionText = Textarea "Форма"
        , optionKey = False
        , optionPoints = 0
        }

    d307_2 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "Б."
        , optionText = Textarea "Размер"
        , optionKey = False
        , optionPoints = 0
        }

    d307_3 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "В."
        , optionText = Textarea "Очень дорого"
        , optionKey = False
        , optionPoints = 0
        }

    k307 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "Г."
        , optionText = Textarea "неосязаемость"
        , optionKey = True
        , optionPoints = 1
        }

    q308 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 8
               , stemText = Textarea "Какая из следующих фраз отражает концепцию маркетинга?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d308_1 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "А."
        , optionText = Textarea "Поставщик – король на рынке"
        , optionKey = False
        , optionPoints = 0
        }

    d308_2 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "Б."
        , optionText = Textarea "Маркетинг следует рассматривать как охоту, а не садоводство."
        , optionKey = False
        , optionPoints = 0
        }

    d308_3 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "В."
        , optionText = Textarea "Это то, что я делаю, не могли бы вы купить его"
        , optionKey = False
        , optionPoints = 0
        }

    k308 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "Г."
        , optionText = Textarea "Это то, чего я хочу, пожалуйста, сделай это."
        , optionKey = True
        , optionPoints = 1
        }

    q309 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 9
               , stemText = Textarea "Решение ценовой конкуренции заключается в разработке дифференцированного:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d309_1 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "А."
        , optionText = Textarea "Продукт, цена и продвижение"
        , optionKey = False
        , optionPoints = 0
        }

    k309 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "Б."
        , optionText = Textarea "Предложение, доставка и изображение"
        , optionKey = True
        , optionPoints = 1
        }

    d309_2 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "В."
        , optionText = Textarea "Упаковка и этикетка"
        , optionKey = False
        , optionPoints = 0
        }

    d309_3 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "Г."
        , optionText = Textarea "Международный веб-сайт"
        , optionKey = False
        , optionPoints = 0
        }

    q310 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 10
               , stemText = Textarea "Вы регулярно покупаете чистящие средства для помощи попечителям. Это показывает, какая ситуация покупки?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d310_1 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "А."
        , optionText = Textarea "Модифицированный ребай"
        , optionKey = False
        , optionPoints = 0
        }

    k310 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "Б."
        , optionText = Textarea "Прямой ребай"
        , optionKey = True
        , optionPoints = 1
        }

    d310_2 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "В."
        , optionText = Textarea "Модифицированный прямой ребай"
        , optionKey = False
        , optionPoints = 0
        }

    d310_3 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "Г."
        , optionText = Textarea "Потребительская покупка"
        , optionKey = False
        , optionPoints = 0
        }

    q311 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 11
               , stemText = Textarea "К какому из следующих вариантов относится оценка покупателем разницы между всеми выгодами и всеми затратами маркетингового предложения по сравнению с конкурирующими предложениями?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k311 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "А."
        , optionText = Textarea "Воспринимаемая клиентом ценность"
        , optionKey = True
        , optionPoints = 1
        }

    d311_1 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "Б."
        , optionText = Textarea "Маркетинговая близорукость"
        , optionKey = False
        , optionPoints = 0
        }

    d311_2 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "В."
        , optionText = Textarea "Управление взаимоотношениями с клиентами"
        , optionKey = False
        , optionPoints = 0
        }

    d311_3 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "Г."
        , optionText = Textarea "Удовлетворенность клиентов"
        , optionKey = False
        , optionPoints = 0
        }

    q312 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 12
               , stemText = Textarea "Покупка товаров и услуг для дальнейшей переработки или использования в производственном процессе относится к какому из следующих рынков?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d312_1 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "А."
        , optionText = Textarea "Потребительские рынки"
        , optionKey = False
        , optionPoints = 0
        }

    d312_2 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "Б."
        , optionText = Textarea "Государственные рынки"
        , optionKey = False
        , optionPoints = 0
        }

    k312 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "В."
        , optionText = Textarea "Деловые рынки"
        , optionKey = True
        , optionPoints = 1
        }

    d312_3 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "Г."
        , optionText = Textarea "Международные рынки"
        , optionKey = False
        , optionPoints = 0
        }

    q313 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 13
               , stemText = Textarea "Разработчики продуктов должны думать о продуктах и услугах на трех уровнях. Каждый уровень производит больше ценностей для клиента. Какой из следующих уровней является наиболее базовым, отвечающим на вопрос: «Что на самом деле делает покупка покупателем?»"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d313_1 <- insert $ Option
        { optionStem = q313
        , optionOrdinal = "А."
        , optionText = Textarea "Фактический продукт"
        , optionKey = False
        , optionPoints = 0
        }

    d313_2 <- insert $ Option
        { optionStem = q313
        , optionOrdinal = "Б."
        , optionText = Textarea "Дополненный продукт"
        , optionKey = False
        , optionPoints = 0
        }

    k313 <- insert $ Option
        { optionStem = q313
        , optionOrdinal = "В."
        , optionText = Textarea "Основное преимущество"
        , optionKey = True
        , optionPoints = 1
        }

    d313_3 <- insert $ Option
        { optionStem = q313
        , optionOrdinal = "Г."
        , optionText = Textarea "Кобрендинг"
        , optionKey = False
        , optionPoints = 0
        }

    q314 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 14
               , stemText = Textarea "Психическое действие, состояние или привычка доверять или доверять другому показывает, какой из следующих вариантов?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d314_1 <- insert $ Option
        { optionStem = q314
        , optionOrdinal = "А."
        , optionText = Textarea "Мотив"
        , optionKey = False
        , optionPoints = 0
        }

    k314 <- insert $ Option
        { optionStem = q314
        , optionOrdinal = "Б."
        , optionText = Textarea "Вера"
        , optionKey = True
        , optionPoints = 1
        }

    d314_2 <- insert $ Option
        { optionStem = q314
        , optionOrdinal = "В."
        , optionText = Textarea "поведение"
        , optionKey = False
        , optionPoints = 0
        }

    d314_3 <- insert $ Option
        { optionStem = q314
        , optionOrdinal = "Г."
        , optionText = Textarea "Отношение"
        , optionKey = False
        , optionPoints = 0
        }

    q315 <- insert $ Stem
               { stemTest = e301
               , stemSkill = s301
               , stemOrdinal = 15
               , stemText = Textarea "Как потребители реагируют на различные маркетинговые усилия, которые может использовать компания? Что является отправной точкой поведения покупателя?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d315_1 <- insert $ Option
        { optionStem = q315
        , optionOrdinal = "А."
        , optionText = Textarea "Вера"
        , optionKey = False
        , optionPoints = 0
        }

    d315_2 <- insert $ Option
        { optionStem = q315
        , optionOrdinal = "Б."
        , optionText = Textarea "Субкультура"
        , optionKey = False
        , optionPoints = 0
        }

    d315_3 <- insert $ Option
        { optionStem = q315
        , optionOrdinal = "В."
        , optionText = Textarea "Ощущение после покупки"
        , optionKey = False
        , optionPoints = 0
        }

    k315 <- insert $ Option
        { optionStem = q315
        , optionOrdinal = "Г."
        , optionText = Textarea "Модель стимул-реакция"
        , optionKey = True
        , optionPoints = 1
        }

    r301 <- insert $ Exam
        { examTest = e301
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7924) now
        , examEnd = pure $ addUTCTime (-7908) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,d303_3)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r301
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r302 <- insert $ Exam
        { examTest = e301
        , examCandidate = c002
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7030) now
        , examEnd = pure $ addUTCTime (-7001) now
        }

    forM_ [ (q301,d301_1)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,d311_2)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,d315_3)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r302
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r303 <- insert $ Exam
        { examTest = e301
        , examCandidate = c003
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-6035) now
        , examEnd = pure $ addUTCTime (-6007) now
        }

    forM_ [ (q301,d301_1)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,d310_2)
          , (q311,k311)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r303
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r304 <- insert $ Exam
        { examTest = e301
        , examCandidate = c004
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-6025) now
        , examEnd = pure $ addUTCTime (-6004) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,d311_3)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r304
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r305 <- insert $ Exam
        { examTest = e301
        , examCandidate = c005
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-7025) now
        , examEnd = pure $ addUTCTime (-7003) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,d303_2)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,d308_3)
          , (q309,k309)
          , (q310,k310)
          , (q311,d311_3)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r305
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r306 <- insert $ Exam
        { examTest = e301
        , examCandidate = c006
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-8020) now
        , examEnd = pure $ addUTCTime (-8000) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,d308_3)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          , (q313,k313)
          , (q314,d314_3)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r306
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r307 <- insert $ Exam
        { examTest = e301
        , examCandidate = c007
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-9027) now
        , examEnd = pure $ addUTCTime (-9000) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r307
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r308 <- insert $ Exam
        { examTest = e301
        , examCandidate = c008
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5028) now
        , examEnd = pure $ addUTCTime (-5000) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,d304_3)
          , (q305,k305)
          , (q306,d306_3)
          , (q307,d307_1)
          , (q308,d308_2)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r308
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r309 <- insert $ Exam
        { examTest = e301
        , examCandidate = c009
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4029) now
        , examEnd = pure $ addUTCTime (-4003) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,d304_3)
          , (q305,k305)
          , (q306,d306_3)
          , (q307,k307)
          , (q308,d308_2)
          , (q309,k309)
          , (q310,d310_1)
          , (q311,k311)
          , (q312,d312_2)
          , (q313,d313_3)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r309
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r310 <- insert $ Exam
        { examTest = e301
        , examCandidate = c010
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-3028) now
        , examEnd = pure $ addUTCTime (-3002) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,k305)
          , (q306,d306_3)
          , (q307,k307)
          , (q308,d308_2)
          , (q309,k309)
          , (q310,d310_1)
          , (q311,d311_1)
          , (q312,d312_2)
          , (q313,k313)
          , (q314,k314)
          , (q315,k315)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r310
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r311 <- insert $ Exam
        { examTest = e301
        , examCandidate = c011
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-2027) now
        , examEnd = pure $ addUTCTime (-2003) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,k304)
          , (q305,d305_2)
          , (q306,d306_3)
          , (q307,k307)
          , (q308,d308_2)
          , (q309,k309)
          , (q310,d310_1)
          , (q311,k311)
          , (q312,d312_2)
          , (q313,k313)
          , (q314,k314)
          , (q315,d315_3)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r311
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s401 <- insert $ Skill
        { skillCode = "Стратегический менеджмент"
        , skillName = "Стратегический менеджмент"
        , skillDescr = Just "Навыки стратегического управления"
        }

    e401 <- insert $ Test
        { testCode = "Э401"
        , testName = "Стратегический менеджмент"
        , testDuration = 30
        , testDurationUnit = TimeUnitMinute
        , testPass = 10
        , testDescr = Just $ Textarea "Экзамен по стратегическому менеджменту"
        , testState = TestStatePublished
        }

    q401 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 1
               , stemText = Textarea "Стратегия разрабатывается дальновидным руководителем в ___________ режиме стратегического управления"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d401_1 <- insert $ Option
        { optionStem = q401
        , optionOrdinal = "А."
        , optionText = Textarea "режим планирования"
        , optionKey = False
        , optionPoints = 0
        }

    d401_2 <- insert $ Option
        { optionStem = q401
        , optionOrdinal = "Б."
        , optionText = Textarea "адаптивный режим"
        , optionKey = False
        , optionPoints = 0
        }

    d401_3 <- insert $ Option
        { optionStem = q401
        , optionOrdinal = "В."
        , optionText = Textarea "стратегический режим"
        , optionKey = False
        , optionPoints = 0
        }

    k401 <- insert $ Option
        { optionStem = q401
        , optionOrdinal = "Г."
        , optionText = Textarea "предпринимательский режим"
        , optionKey = True
        , optionPoints = 1
        }

    q402 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 2
               , stemText = Textarea "Стратегия стабильности – это ____________ стратегия"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k402 <- insert $ Option
        { optionStem = q402
        , optionOrdinal = "А."
        , optionText = Textarea "корпоративный уровень"
        , optionKey = True
        , optionPoints = 1
        }

    d402_1 <- insert $ Option
        { optionStem = q402
        , optionOrdinal = "Б."
        , optionText = Textarea "бизнес-уровень"
        , optionKey = False
        , optionPoints = 0
        }

    d402_2 <- insert $ Option
        { optionStem = q402
        , optionOrdinal = "В."
        , optionText = Textarea "функциональный уровень"
        , optionKey = False
        , optionPoints = 0
        }

    d402_3 <- insert $ Option
        { optionStem = q402
        , optionOrdinal = "Г."
        , optionText = Textarea "стратегический уровень"
        , optionKey = False
        , optionPoints = 0
        }

    q403 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 3
               , stemText = Textarea "Какими средствами будут достигнуты долгосрочные цели?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k403 <- insert $ Option
        { optionStem = q403
        , optionOrdinal = "А."
        , optionText = Textarea "Стратегии"
        , optionKey = True
        , optionPoints = 1
        }

    d403_1 <- insert $ Option
        { optionStem = q403
        , optionOrdinal = "Б."
        , optionText = Textarea "Политики"
        , optionKey = False
        , optionPoints = 0
        }

    d403_2 <- insert $ Option
        { optionStem = q403
        , optionOrdinal = "В."
        , optionText = Textarea "Сила"
        , optionKey = False
        , optionPoints = 0
        }

    d403_3 <- insert $ Option
        { optionStem = q403
        , optionOrdinal = "Г."
        , optionText = Textarea "Возможности"
        , optionKey = False
        , optionPoints = 0
        }

    q404 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 4
               , stemText = Textarea "Маркетинговая стратегия – это ___________ тип стратегии."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k404 <- insert $ Option
        { optionStem = q404
        , optionOrdinal = "А."
        , optionText = Textarea "бизнес-уровень"
        , optionKey = False
        , optionPoints = 0
        }

    d404_2 <- insert $ Option
        { optionStem = q404
        , optionOrdinal = "Б."
        , optionText = Textarea "Стратегия роста"
        , optionKey = False
        , optionPoints = 0
        }

    d404_3 <- insert $ Option
        { optionStem = q404
        , optionOrdinal = "В."
        , optionText = Textarea "Корпоративная стратегия"
        , optionKey = False
        , optionPoints = 0
        }

    k404 <- insert $ Option
        { optionStem = q404
        , optionOrdinal = "Г."
        , optionText = Textarea "функциональная стратегия"
        , optionKey = True
        , optionPoints = 1
        }

    q405 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 5
               , stemText = Textarea "Когда отрасль в значительной степени зависит от государственных контрактов, какие прогнозы могут быть наиболее важной частью внешнего аудита."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d405_1 <- insert $ Option
        { optionStem = q405
        , optionOrdinal = "А."
        , optionText = Textarea "Экономический"
        , optionKey = False
        , optionPoints = 0
        }

    d405_2 <- insert $ Option
        { optionStem = q405
        , optionOrdinal = "Б."
        , optionText = Textarea "Конкурентоспособный"
        , optionKey = False
        , optionPoints = 0
        }

    k405 <- insert $ Option
        { optionStem = q405
        , optionOrdinal = "В."
        , optionText = Textarea "Политический"
        , optionKey = True
        , optionPoints = 1
        }

    d405_3 <- insert $ Option
        { optionStem = q405
        , optionOrdinal = "Г."
        , optionText = Textarea "Многонациональный"
        , optionKey = False
        , optionPoints = 0
        }

    q406 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 6
               , stemText = Textarea "Возможное и желаемое будущее состояние организации называется:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d406_1 <- insert $ Option
        { optionStem = q406
        , optionOrdinal = "А."
        , optionText = Textarea "Миссия"
        , optionKey = False
        , optionPoints = 0
        }

    k406 <- insert $ Option
        { optionStem = q406
        , optionOrdinal = "Б."
        , optionText = Textarea "Зрение"
        , optionKey = True
        , optionPoints = 1
        }

    d406_2 <- insert $ Option
        { optionStem = q406
        , optionOrdinal = "В."
        , optionText = Textarea "Реализация стратегии"
        , optionKey = False
        , optionPoints = 0
        }

    d406_3 <- insert $ Option
        { optionStem = q406
        , optionOrdinal = "Г."
        , optionText = Textarea "Формулировка стратегии"
        , optionKey = False
        , optionPoints = 0
        }

    q407 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 7
               , stemText = Textarea "Что символизирует вопросительный знак в матрице BCG?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k407 <- insert $ Option
        { optionStem = q407
        , optionOrdinal = "А."
        , optionText = Textarea "Оставайтесь диверсифицированным"
        , optionKey = True
        , optionPoints = 1
        }

    d407_1 <- insert $ Option
        { optionStem = q407
        , optionOrdinal = "Б."
        , optionText = Textarea "Вкладывать деньги"
        , optionKey = False
        , optionPoints = 0
        }

    d407_2 <- insert $ Option
        { optionStem = q407
        , optionOrdinal = "В."
        , optionText = Textarea "Стабильный"
        , optionKey = False
        , optionPoints = 0
        }

    d407_3 <- insert $ Option
        { optionStem = q407
        , optionOrdinal = "Г."
        , optionText = Textarea "Ликвидировать"
        , optionKey = False
        , optionPoints = 0
        }

    q408 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 8
               , stemText = Textarea "Продажа всех активов компании по частям за их материальную стоимость называется:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d408_1 <- insert $ Option
        { optionStem = q408
        , optionOrdinal = "А."
        , optionText = Textarea "Отчуждение"
        , optionKey = False
        , optionPoints = 0
        }

    d408_2 <- insert $ Option
        { optionStem = q408
        , optionOrdinal = "Б."
        , optionText = Textarea "Концентрическая диверсификация"
        , optionKey = False
        , optionPoints = 0
        }

    k408 <- insert $ Option
        { optionStem = q408
        , optionOrdinal = "В."
        , optionText = Textarea "Ликвидация"
        , optionKey = True
        , optionPoints = 1
        }

    d408_3 <- insert $ Option
        { optionStem = q408
        , optionOrdinal = "Г."
        , optionText = Textarea "Несвязанная интеграция"
        , optionKey = False
        , optionPoints = 0
        }

    q409 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 9
               , stemText = Textarea "Что символизируют дойные коровы в матрице БКГ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d409_1 <- insert $ Option
        { optionStem = q409
        , optionOrdinal = "А."
        , optionText = Textarea "Оставайтесь диверсифицированным"
        , optionKey = False
        , optionPoints = 0
        }

    d409_2 <- insert $ Option
        { optionStem = q409
        , optionOrdinal = "Б."
        , optionText = Textarea "Вкладывать деньги"
        , optionKey = False
        , optionPoints = 0
        }

    k409 <- insert $ Option
        { optionStem = q409
        , optionOrdinal = "В."
        , optionText = Textarea "Стабильный"
        , optionKey = True
        , optionPoints = 1
        }

    d409_3 <- insert $ Option
        { optionStem = q409
        , optionOrdinal = "Г."
        , optionText = Textarea "Ликвидировать"
        , optionKey = False
        , optionPoints = 0
        }

    q410 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 10
               , stemText = Textarea "Матрица BCG основана на"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d410_1 <- insert $ Option
        { optionStem = q410
        , optionOrdinal = "А."
        , optionText = Textarea "Привлекательность отрасли и сила бизнеса"
        , optionKey = False
        , optionPoints = 0
        }

    d410_2 <- insert $ Option
        { optionStem = q410
        , optionOrdinal = "Б."
        , optionText = Textarea "Темпы роста отрасли и сила бизнеса"
        , optionKey = False
        , optionPoints = 0
        }

    d410_3 <- insert $ Option
        { optionStem = q410
        , optionOrdinal = "В."
        , optionText = Textarea "Привлекательность отрасли и относительная доля рынка"
        , optionKey = False
        , optionPoints = 0
        }

    k410 <- insert $ Option
        { optionStem = q410
        , optionOrdinal = "Г."
        , optionText = Textarea "Темпы роста отрасли и относительная доля рынка"
        , optionKey = True
        , optionPoints = 1
        }

    q411 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 11
               , stemText = Textarea "Каковы ориентиры для принятия решений?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d411_1 <- insert $ Option
        { optionStem = q411
        , optionOrdinal = "А."
        , optionText = Textarea "Правила"
        , optionKey = False
        , optionPoints = 0
        }

    d411_2 <- insert $ Option
        { optionStem = q411
        , optionOrdinal = "Б."
        , optionText = Textarea "Процедуры"
        , optionKey = False
        , optionPoints = 0
        }

    d411_3 <- insert $ Option
        { optionStem = q411
        , optionOrdinal = "В."
        , optionText = Textarea "Цели"
        , optionKey = False
        , optionPoints = 0
        }

    k411 <- insert $ Option
        { optionStem = q411
        , optionOrdinal = "Г."
        , optionText = Textarea "Политики"
        , optionKey = True
        , optionPoints = 1
        }

    q412 <- insert $ Stem
               { stemTest = e401
               , stemSkill = s401
               , stemOrdinal = 12
               , stemText = Textarea "В стратегическом мышлении, какова приблизительно долгосрочная перспектива?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d412_1 <- insert $ Option
        { optionStem = q412
        , optionOrdinal = "А."
        , optionText = Textarea "от 1 месяца до 1 года"
        , optionKey = False
        , optionPoints = 0
        }

    d412_2 <- insert $ Option
        { optionStem = q412
        , optionOrdinal = "Б."
        , optionText = Textarea "от 2 до 3 лет"
        , optionKey = False
        , optionPoints = 0
        }

    d412_3 <- insert $ Option
        { optionStem = q412
        , optionOrdinal = "В."
        , optionText = Textarea "от 3 до 5 лет"
        , optionKey = False
        , optionPoints = 0
        }

    k412 <- insert $ Option
        { optionStem = q412
        , optionOrdinal = "Г."
        , optionText = Textarea "Более 5 лет"
        , optionKey = True
        , optionPoints = 1
        }

    r401 <- insert $ Exam
        { examTest = e401
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-2027) now
        , examEnd = pure $ addUTCTime (-2003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,d405_2)
          , (q406,d406_3)
          , (q407,k407)
          , (q408,d408_2)
          , (q409,k409)
          , (q410,d410_1)
          , (q411,k411)
          , (q412,d412_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r401
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r402 <- insert $ Exam
        { examTest = e401
        , examCandidate = c002
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-3027) now
        , examEnd = pure $ addUTCTime (-3003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,d406_3)
          , (q407,k407)
          , (q408,d408_2)
          , (q409,k409)
          , (q410,d410_1)
          , (q411,k411)
          , (q412,d412_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r402
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r404 <- insert $ Exam
        { examTest = e401
        , examCandidate = c004
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4027) now
        , examEnd = pure $ addUTCTime (-4003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,k406)
          , (q407,k407)
          , (q408,d408_2)
          , (q409,k409)
          , (q410,d410_1)
          , (q411,k411)
          , (q412,d412_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r404
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r406 <- insert $ Exam
        { examTest = e401
        , examCandidate = c006
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-5027) now
        , examEnd = pure $ addUTCTime (-5003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,k406)
          , (q407,k407)
          , (q408,k408)
          , (q409,k409)
          , (q410,d410_1)
          , (q411,k411)
          , (q412,d412_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r406
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r408 <- insert $ Exam
        { examTest = e401
        , examCandidate = c008
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-3027) now
        , examEnd = pure $ addUTCTime (-3003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,k406)
          , (q407,k407)
          , (q408,k408)
          , (q409,k409)
          , (q410,k410)
          , (q411,k411)
          , (q412,d412_2)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r408
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r409 <- insert $ Exam
        { examTest = e401
        , examCandidate = c009
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-9027) now
        , examEnd = pure $ addUTCTime (-9003) now
        }

    forM_ [ (q401,k401)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,k406)
          , (q407,k407)
          , (q408,k408)
          , (q409,k409)
          , (q410,k410)
          , (q411,k411)
          , (q412,k412)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r409
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r411 <- insert $ Exam
        { examTest = e401
        , examCandidate = c011
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-11027) now
        , examEnd = pure $ addUTCTime (-11003) now
        }

    forM_ [ (q401,d401_1)
          , (q402,k402)
          , (q403,k403)
          , (q404,k404)
          , (q405,k405)
          , (q406,k406)
          , (q407,k407)
          , (q408,k408)
          , (q409,k409)
          , (q410,k410)
          , (q411,k411)
          , (q412,k412)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r411
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s501 <- insert $ Skill
        { skillCode = "Базовая химическая инженерия"
        , skillName = "Базовая химическая инженерия"
        , skillDescr = Just "Навыки базовой химической инженерии"
        }

    t501 <- insert $ Test
        { testCode = "Э502"
        , testName = "Химическая инженерия"
        , testDuration = 20
        , testDurationUnit = TimeUnitMinute
        , testPass = 8
        , testDescr = Just $ Textarea "Проверяет базовые навыки химической инженерии"
        , testState = TestStatePublished
        }

    q501 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 1
               , stemText = Textarea "Что является единицей измерения удельного веса?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k501 <- insert $ Option
        { optionStem = q501
        , optionOrdinal = "а)"
        , optionText = Textarea "Безразмерный"
        , optionKey = True
        , optionPoints = 1
        }

    d501_1 <- insert $ Option
        { optionStem = q501
        , optionOrdinal = "б)"
        , optionText = Textarea "м/с<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d501_2 <- insert $ Option
        { optionStem = q501
        , optionOrdinal = "в)"
        , optionText = Textarea "Н/м<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    d501_3 <- insert $ Option
        { optionStem = q501
        , optionOrdinal = "г)"
        , optionText = Textarea "кг/м<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    q502 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 2
               , stemText = Textarea "Что из следующего имеет то же количество молей, что и 398 граммов CuSO<sub>4</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d502_1 <- insert $ Option
        { optionStem = q502
        , optionOrdinal = "а)"
        , optionText = Textarea "35 грамм азота"
        , optionKey = False
        , optionPoints = 0
        }

    d502_2 <- insert $ Option
        { optionStem = q502
        , optionOrdinal = "б)"
        , optionText = Textarea "58,5 г хлорида натрия"
        , optionKey = False
        , optionPoints = 0
        }

    d502_3 <- insert $ Option
        { optionStem = q502
        , optionOrdinal = "в)"
        , optionText = Textarea "2 грамма водорода"
        , optionKey = False
        , optionPoints = 0
        }
        
    k502 <- insert $ Option
        { optionStem = q502
        , optionOrdinal = "г)"
        , optionText = Textarea "40 грамм кислорода"
        , optionKey = True
        , optionPoints = 1
        }

    q503 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 3
               , stemText = Textarea "Каков удельный вес 5 кг воды в 10 м<sup>3</sup> по отношению к 500 г/м<sup>3</sup>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d503_1 <- insert $ Option
        { optionStem = q503
        , optionOrdinal = "а)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    d503_2 <- insert $ Option
        { optionStem = q503
        , optionOrdinal = "б)"
        , optionText = Textarea "5"
        , optionKey = False
        , optionPoints = 0
        }

    d503_3 <- insert $ Option
        { optionStem = q503
        , optionOrdinal = "в)"
        , optionText = Textarea "0,5"
        , optionKey = False
        , optionPoints = 0
        }
        
    k503 <- insert $ Option
        { optionStem = q503
        , optionOrdinal = "г)"
        , optionText = Textarea "1"
        , optionKey = True
        , optionPoints = 1
        }

    q504 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 4
               , stemText = Textarea "Что является единицей мольной доли?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d504_1 <- insert $ Option
        { optionStem = q504
        , optionOrdinal = "а)"
        , optionText = Textarea "Н/м<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d504_2 <- insert $ Option
        { optionStem = q504
        , optionOrdinal = "б)"
        , optionText = Textarea "м<sup>-2</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d504_3 <- insert $ Option
        { optionStem = q504
        , optionOrdinal = "в)"
        , optionText = Textarea "кг/м<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    k504 <- insert $ Option
        { optionStem = q504
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из упомянутых"
        , optionKey = True
        , optionPoints = 1
        }

    q505 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 5
               , stemText = Textarea "Какова масса 10 моль смеси состава 15% O<sub>2</sub>, 25% SO<sub>2</sub>, 30% COCl<sub>2</sub>, 25% SO<sub>3</sub> и 5% N<sub>2</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d505_1 <- insert $ Option
        { optionStem = q505
        , optionOrdinal = "а)"
        , optionText = Textarea "564"
        , optionKey = False
        , optionPoints = 0
        }

    d505_2 <- insert $ Option
        { optionStem = q505
        , optionOrdinal = "б)"
        , optionText = Textarea "475"
        , optionKey = False
        , optionPoints = 0
        }

    d505_3 <- insert $ Option
        { optionStem = q505
        , optionOrdinal = "в)"
        , optionText = Textarea "867"
        , optionKey = False
        , optionPoints = 0
        }
        
    k505 <- insert $ Option
        { optionStem = q505
        , optionOrdinal = "г)"
        , optionText = Textarea "719"
        , optionKey = True
        , optionPoints = 1
        }

    q506 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 6
               , stemText = Textarea "Что такое 100<sup>o</sup>C в градусах Фаренгейта?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d506_1 <- insert $ Option
        { optionStem = q506
        , optionOrdinal = "а)"
        , optionText = Textarea "100<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    k506 <- insert $ Option
        { optionStem = q506
        , optionOrdinal = "б)"
        , optionText = Textarea "212<sup>o</sup>F"
        , optionKey = True
        , optionPoints = 1
        }

    d506_2 <- insert $ Option
        { optionStem = q506
        , optionOrdinal = "в)"
        , optionText = Textarea "460<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }
        
    d506_3 <- insert $ Option
        { optionStem = q506
        , optionOrdinal = "г)"
        , optionText = Textarea "0<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    q507 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 7
               , stemText = Textarea "Каково давление 1900 Торр в баре?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d507_1 <- insert $ Option
        { optionStem = q507
        , optionOrdinal = "а)"
        , optionText = Textarea "2,46"
        , optionKey = False
        , optionPoints = 0
        }

    d507_2 <- insert $ Option
        { optionStem = q507
        , optionOrdinal = "б)"
        , optionText = Textarea "2,87"
        , optionKey = False
        , optionPoints = 0
        }

    k507 <- insert $ Option
        { optionStem = q507
        , optionOrdinal = "в)"
        , optionText = Textarea "2,40"
        , optionKey = True
        , optionPoints = 1
        }
        
    d507_3 <- insert $ Option
        { optionStem = q507
        , optionOrdinal = "г)"
        , optionText = Textarea "2,68"
        , optionKey = False
        , optionPoints = 0
        }

    q508 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 8
               , stemText = Textarea "Что из перечисленного не является прибором для измерения давления?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k508 <- insert $ Option
        { optionStem = q508
        , optionOrdinal = "а)"
        , optionText = Textarea "Гальванометр"
        , optionKey = True
        , optionPoints = 1
        }

    d508_1 <- insert $ Option
        { optionStem = q508
        , optionOrdinal = "б)"
        , optionText = Textarea "Манометр"
        , optionKey = False
        , optionPoints = 0
        }

    d508_2 <- insert $ Option
        { optionStem = q508
        , optionOrdinal = "в)"
        , optionText = Textarea "Барометр"
        , optionKey = False
        , optionPoints = 0
        }
        
    d508_3 <- insert $ Option
        { optionStem = q508
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из упомянутых"
        , optionKey = False
        , optionPoints = 0
        }

    q509 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 9
               , stemText = Textarea "Что из следующего используется для измерения давления только жидкости?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d509_1 <- insert $ Option
        { optionStem = q509
        , optionOrdinal = "а)"
        , optionText = Textarea "Дифференциальный манометр"
        , optionKey = False
        , optionPoints = 0
        }

    d509_2 <- insert $ Option
        { optionStem = q509
        , optionOrdinal = "б)"
        , optionText = Textarea "Манометр"
        , optionKey = False
        , optionPoints = 0
        }

    k509 <- insert $ Option
        { optionStem = q509
        , optionOrdinal = "в)"
        , optionText = Textarea "Пьезометр"
        , optionKey = True
        , optionPoints = 1
        }
        
    d509_3 <- insert $ Option
        { optionStem = q509
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из упомянутых"
        , optionKey = False
        , optionPoints = 0
        }

    q510 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 10
               , stemText = Textarea "Что из нижеперечисленного является функцией состояния?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    k510 <- insert $ Option
        { optionStem = q510
        , optionOrdinal = "а)"
        , optionText = Textarea "Энтропия"
        , optionKey = True
        , optionPoints = 1
        }

    d510_1 <- insert $ Option
        { optionStem = q510
        , optionOrdinal = "б)"
        , optionText = Textarea "Тепло"
        , optionKey = False
        , optionPoints = 0
        }

    d510_2 <- insert $ Option
        { optionStem = q510
        , optionOrdinal = "в)"
        , optionText = Textarea "Работа"
        , optionKey = False
        , optionPoints = 0
        }
        
    d510_3 <- insert $ Option
        { optionStem = q510
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из упомянутых"
        , optionKey = False
        , optionPoints = 0
        }

    q511 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 11
               , stemText = Textarea "Кипение воды в сосуде является примером чего из нижеперечисленного?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d511_1 <- insert $ Option
        { optionStem = q511
        , optionOrdinal = "а)"
        , optionText = Textarea "Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }

    d511_2 <- insert $ Option
        { optionStem = q511
        , optionOrdinal = "б)"
        , optionText = Textarea "Batch"
        , optionKey = False
        , optionPoints = 0
        }

    d511_3 <- insert $ Option
        { optionStem = q511
        , optionOrdinal = "в)"
        , optionText = Textarea "Batch и Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }
        
    k511 <- insert $ Option
        { optionStem = q511
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из вышеперечисленных"
        , optionKey = True
        , optionPoints = 1
        }

    q512 <- insert $ Stem
               { stemTest = t501
               , stemSkill = s501
               , stemOrdinal = 12
               , stemText = Textarea "Что из следующего верно относительно лимитирующих реагентов?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Выберите один"
               }

    d512_1 <- insert $ Option
        { optionStem = q512
        , optionOrdinal = "а)"
        , optionText = Textarea "Потребляет частично"
        , optionKey = False
        , optionPoints = 0
        }

    d512_2 <- insert $ Option
        { optionStem = q512
        , optionOrdinal = "б)"
        , optionText = Textarea "Не вступает в реакцию"
        , optionKey = False
        , optionPoints = 0
        }

    k512 <- insert $ Option
        { optionStem = q512
        , optionOrdinal = "в)"
        , optionText = Textarea "Полностью потребляет"
        , optionKey = True
        , optionPoints = 1
        }
        
    d512_3 <- insert $ Option
        { optionStem = q512
        , optionOrdinal = "г)"
        , optionText = Textarea "Ни один из упомянутых"
        , optionKey = False
        , optionPoints = 0
        }

    e502 <- insert $ Exam
        { examTest = t501
        , examCandidate = c002
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-4015) now
        , examEnd = pure $ addUTCTime (-4005) now
        }

    forM_ [ (q501,d501_1)
          , (q502,d501_2)
          , (q503,d503_3)
          , (q504,k504)
          , (q505,d505_3)
          , (q506,k506)
          , (q507,k507)
          , (q508,k508)
          , (q509,k509)
          , (q510,d510_1)
          , (q511,k511)
          , (q512,k512)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e502
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e503 <- insert $ Exam
        { examTest = t501
        , examCandidate = c004
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-2015) now
        , examEnd = pure $ addUTCTime (-2005) now
        }

    forM_ [ (q501,d501_1)
          , (q502,d501_2)
          , (q503,k503)
          , (q504,k504)
          , (q505,d505_3)
          , (q506,k506)
          , (q507,k507)
          , (q508,d508_1)
          , (q509,k509)
          , (q510,k510)
          , (q511,k511)
          , (q512,k512)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e503
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e504 <- insert $ Exam
        { examTest = t501
        , examCandidate = c006
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-1013) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (q501,k501)
          , (q502,k502)
          , (q503,k503)
          , (q504,k504)
          , (q505,d505_3)
          , (q506,k506)
          , (q507,d507_3)
          , (q508,k508)
          , (q509,k509)
          , (q510,k510)
          , (q511,k511)
          , (q512,k512)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e504
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e505 <- insert $ Exam
        { examTest = t501
        , examCandidate = c008
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-1020) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (q501,k501)
          , (q502,k502)
          , (q503,d503_2)
          , (q504,k504)
          , (q505,d505_3)
          , (q506,k506)
          , (q507,d507_3)
          , (q508,k508)
          , (q509,k509)
          , (q510,d510_3)
          , (q511,k511)
          , (q512,d512_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e505
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e506 <- insert $ Exam
        { examTest = t501
        , examCandidate = c010
        , examStatus = ExamStatusCompleted
        , examAttempt = 1
        , examStart = addUTCTime (-2021) now
        , examEnd = pure $ addUTCTime (-2002) now
        }

    forM_ [ (q501,k501)
          , (q502,k502)
          , (q503,k503)
          , (q504,k504)
          , (q505,d505_3)
          , (q506,k506)
          , (q507,k507)
          , (q508,k508)
          , (q509,k509)
          , (q510,d510_3)
          , (q511,k511)
          , (q512,d512_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e506
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

