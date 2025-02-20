{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoDataRO (populateRO) where

import ClassyPrelude.Yesod (ReaderT, forM_, Textarea (Textarea))

import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.ByteString as BS (readFile)
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay), addUTCTime)

import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert, insert_) )

import Model
    ( Skill(Skill, skillCode, skillName, skillDescr)
    , Test (Test, testCode, testName, testDescr, testDuration, testPass, testState)
    , Stem (Stem, stemTest, stemSkill, stemOrdinal, stemText, stemInstruc, stemType)
    , Option (optionStem, Option, optionOrdinal, optionText, optionKey, optionPoints)
    , Candidate
      ( Candidate, candidateFamilyName, candidateGivenName, candidateAdditionalName
      , candidateBday, candidateUser
      )
    , Photo (Photo, photoCandidate, photoPhoto, photoMime)
    , StemType (SingleRespose, MultiResponse)
    , Exam (Exam, examTest, examCandidate, examAttempt, examStart, examEnd)
    , Answer (Answer, answerExam, answerStem, answerOption, answerTime)
    , TestState (TestStatePublished)
    , User
      ( User, userEmail, userPassword, userName, userAdmin, userSuper, userAuthType
      , userVerkey, userVerified
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    , AuthenticationType (UserAuthTypePassword)
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)


populateRO :: MonadIO m => ReaderT SqlBackend m ()
populateRO = do
    (now,today) <- liftIO $ getCurrentTime >>= \x -> return (x,utctDay x)

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "apopa"
    uid1 <- insert $ User { userEmail = "apopa@xmailx.ro"
                          , userPassword = Just pass1
                          , userName = Just "Andrei Popa"
                          , userSuper = False
                          , userAdmin = True
                          , userAuthType = UserAuthTypePassword
                          , userVerkey = Nothing
                          , userVerified = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "amradu"
    uid2 <- insert $ User { userEmail = "amradu@xmailx.ro"
                          , userPassword = Just pass2
                          , userName = Just "Ana-Maria Radu"
                          , userSuper = False
                          , userAdmin = False
                          , userAuthType = UserAuthTypePassword
                          , userVerkey = Nothing
                          , userVerified = False
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "avionescu"
    uid3 <- insert $ User { userEmail = "avionescu@xmailx.ro"
                          , userPassword = Just pass3
                          , userName = Just "Alexandru Victor Ionescu"
                          , userSuper = False
                          , userAdmin = False
                          , userAuthType = UserAuthTypePassword
                          , userVerkey = Nothing
                          , userVerified = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "mastoica"
    uid4 <- insert $ User { userEmail = "mastoica@xmailx.ro"
                          , userPassword = Just pass4
                          , userName = Just "Maria Alexandra Stoica"
                          , userSuper = False
                          , userAdmin = False
                          , userAuthType = UserAuthTypePassword
                          , userVerkey = Nothing
                          , userVerified = False
                          }

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    c001 <- insert $ Candidate
               { candidateFamilyName = "Popa"
               , candidateGivenName = "Andrei"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-28) today
               , candidateUser = Just uid1
               }
    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c001
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c002 <- insert $ Candidate
               { candidateFamilyName = "Radu"
               , candidateGivenName = "Ana-Maria"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-26) today
               , candidateUser = Just uid2
               }        
    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c002
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c003 <- insert $ Candidate
               { candidateFamilyName = "Ionescu"
               , candidateGivenName = "Alexandru"
               , candidateAdditionalName = Just "Victor"
               , candidateBday = Just $ addGregorianYearsClip (-21) today
               , candidateUser = Just uid3
               }
    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c003
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c004 <- insert $ Candidate
               { candidateFamilyName = "Stoica"
               , candidateGivenName = "Maria"
               , candidateAdditionalName = Just "Alexandra"
               , candidateBday = Just $ addGregorianYearsClip (-30) today
               , candidateUser = Just uid4
               }
    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c004
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c005 <- insert $ Candidate
               { candidateFamilyName = "Rusu"
               , candidateGivenName = "Stefan"
               , candidateAdditionalName = Just "Alexandru"
               , candidateBday = Just $ addGregorianYearsClip (-32) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c005
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c006 <- insert $ Candidate
               { candidateFamilyName = "Munteanu"
               , candidateGivenName = "David"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_7.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c006
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c007 <- insert $ Candidate
               { candidateFamilyName = "Matei"
               , candidateGivenName = "Andreea"
               , candidateAdditionalName = Just "Alexandra"
               , candidateBday = Just $ addGregorianYearsClip (-35) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c007
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c008 <- insert $ Candidate
               { candidateFamilyName = "Marin"
               , candidateGivenName = "Ioana"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-42) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_8.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c008
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c009 <- insert $ Candidate
               { candidateFamilyName = "Lazar"
               , candidateGivenName = "Mihai"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-46) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_9.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c009
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c010 <- insert $ Candidate
               { candidateFamilyName = "Ciobanu"
               , candidateGivenName = "Ionut"
               , candidateAdditionalName = Just "Stefan"
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_10.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c010
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c011 <- insert $ Candidate
               { candidateFamilyName = "Florea"
               , candidateGivenName = "Ioana"
               , candidateAdditionalName = Just "Maria"
               , candidateBday = Just $ addGregorianYearsClip (-31) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_11.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c011
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    s001 <- insert $ Skill
                { skillCode = "Java SE"
                , skillName = "Java Standard Edition"
                , skillDescr = Just "Abilități de programare în Java Standard Edition"
                }

    t001 <- insert $ Test
        { testCode = "E101"
        , testName = "Bazele programarii Java"
        , testDuration = 120
        , testPass = 25
        , testDescr = Just $ Textarea "Testează abilitățile de bază de programare Java"
        , testState = TestStatePublished
        }

    q001 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 1
               , stemText = Textarea "Cine a inventat limbajul de programare Java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d001_1 <- insert $ Option
        { optionStem = q001
        , optionOrdinal = "a)"
        , optionText = Textarea "Guido van Rossum"
        , optionKey = False
        , optionPoints = 0
        }

    k001 <- insert $ Option
        { optionStem = q001
        , optionOrdinal = "b)"
        , optionText = Textarea "James Gosling"
        , optionKey = True
        , optionPoints = 3
        }

    d001_2 <- insert $ Option
        { optionStem = q001
        , optionOrdinal = "c)"
        , optionText = Textarea "Dennis Ritchie"
        , optionKey = False
        , optionPoints = 0
        }

    d001_3 <- insert $ Option
        { optionStem = q001
        , optionOrdinal = "d)"
        , optionText = Textarea "Bjarne Stroustrup"
        , optionKey = False
        , optionPoints = 0
        }

    q002 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 2
               , stemText = Textarea "Care afirmație este adevărată despre Java?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Selectați toate corecte"
               }

    d002_1 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "a)"
        , optionText = Textarea "Java este un limbaj de programare dependent de secvențe"
        , optionKey = False
        , optionPoints = 0
        }

    d002_2 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "b)"
        , optionText = Textarea "Java este un limbaj de programare dependent de cod"
        , optionKey = False
        , optionPoints = 0
        }

    k002_1 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "c)"
        , optionText = Textarea "Java este un limbaj de programare dependent de platformă"
        , optionKey = True
        , optionPoints = 2
        }

    k002_2 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "d)"
        , optionText = Textarea "Java este un limbaj de programare independent de platformă"
        , optionKey = True
        , optionPoints = 5
        }

    q003 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 3
               , stemText = Textarea "Ce componentă este folosită pentru a compila, depana și executa programele java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d003_1 <- insert $ Option
        { optionStem = q003
        , optionOrdinal = "a)"
        , optionText = Textarea "JRE"
        , optionKey = False
        , optionPoints = 0
        }

    d003_2 <- insert $ Option
        { optionStem = q003
        , optionOrdinal = "b)"
        , optionText = Textarea "JIT"
        , optionKey = False
        , optionPoints = 0
        }

    k003 <- insert $ Option
        { optionStem = q003
        , optionOrdinal = "c)"
        , optionText = Textarea "JDK"
        , optionKey = True
        , optionPoints = 3
        }

    d003_3 <- insert $ Option
        { optionStem = q003
        , optionOrdinal = "d)"
        , optionText = Textarea "JVM"
        , optionKey = False
        , optionPoints = 0
        }

    q004 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 4
               , stemText = Textarea "Numărul de tipuri de date primitive în Java este?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d004_1 <- insert $ Option
        { optionStem = q004
        , optionOrdinal = "a)"
        , optionText = Textarea "6"
        , optionKey = False
        , optionPoints = 0
        }

    d004_2 <- insert $ Option
        { optionStem = q004
        , optionOrdinal = "b)"
        , optionText = Textarea "7"
        , optionKey = False
        , optionPoints = 0
        }

    k004 <- insert $ Option
        { optionStem = q004
        , optionOrdinal = "c)"
        , optionText = Textarea "8"
        , optionKey = True
        , optionPoints = 1
        }

    d004_3 <- insert $ Option
        { optionStem = q004
        , optionOrdinal = "d)"
        , optionText = Textarea "9"
        , optionKey = False
        , optionPoints = 0
        }

    q005 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 5
               , stemText = Textarea "Care este dimensiunea float și double în java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k005 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "a)"
        , optionText = Textarea "32 și 64"
        , optionKey = True
        , optionPoints = 1
        }

    d005_1 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "b)"
        , optionText = Textarea "32 și 32"
        , optionKey = False
        , optionPoints = 0
        }

    d005_2 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "c)"
        , optionText = Textarea "64 și 64"
        , optionKey = False
        , optionPoints = 0
        }

    d005_3 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "d)"
        , optionText = Textarea "64 și 32"
        , optionKey = False
        , optionPoints = 0
        }

    q006 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 6
               , stemText = Textarea "În care dintre cazurile posibile este posibilă conversia automată a tipului?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d006_1 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>Byte</code> în <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k006 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>Int</code> în <code>long</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d006_2 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>Long</code> în <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d006_3 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>Short</code> în <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q007 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 7
               , stemText = Textarea [st|Găsiți rezultatul următorului cod.
<code>
<pre>
  int Integer = 24;
  char String = ‘I’;
  System.out.print(Integer);
  System.out.print(String);
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d007_1 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "a)"
        , optionText = Textarea "Eroare de compilare"
        , optionKey = False
        , optionPoints = 0
        }

    d007_2 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "b)"
        , optionText = Textarea "Aruncă excepție"
        , optionKey = False
        , optionPoints = 0
        }

    d007_3 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>I</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k007 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>24 I</code>"
        , optionKey = True
        , optionPoints = 1
        }

    q008 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 8
               , stemText = Textarea [st|Găsiți rezultatul următorului program.
<code>
<pre>
  public class Solution{
     public static void main(String[] args){
                   short x = 10;
                   x =  x * 5;
                   System.out.print(x);
     }
  }
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d008_1 <- insert $ Option
        { optionStem = q008
        , optionOrdinal = "a)"
        , optionText = Textarea "50"
        , optionKey = False
        , optionPoints = 0
        }

    d008_2 <- insert $ Option
        { optionStem = q008
        , optionOrdinal = "b)"
        , optionText = Textarea "10"
        , optionKey = False
        , optionPoints = 0
        }

    k008 <- insert $ Option
        { optionStem = q008
        , optionOrdinal = "c)"
        , optionText = Textarea "Eroare de compilare"
        , optionKey = True
        , optionPoints = 1
        }

    d008_3 <- insert $ Option
        { optionStem = q008
        , optionOrdinal = "d)"
        , optionText = Textarea "Excepție"
        , optionKey = False
        , optionPoints = 0
        }

    q009 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 9
               , stemText = Textarea "Selectați declarația validă."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d009_1 <- insert $ Option
        { optionStem = q009
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>char[] ch = new char(5)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k009 <- insert $ Option
        { optionStem = q009
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>char[] ch = new char[5]</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d009_2 <- insert $ Option
        { optionStem = q009
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>char[] ch = new char()</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d009_3 <- insert $ Option
        { optionStem = q009
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>char[] ch = new char[]</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q010 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 10
               , stemText = Textarea [st|Găsiți rezultatul următorului program.
<code>
<pre>
public class Solution {
  public static void main(String[] args){
    int[]  x = {120, 200, 016};
    for(int i = 0; i < x.length; i++){
      System.out.print(x[i] + “ “);
    }
  }
}
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d010_1 <- insert $ Option
        { optionStem = q010
        , optionOrdinal = "a)"
        , optionText = Textarea "120 200 016"
        , optionKey = False
        , optionPoints = 0
        }

    k010 <- insert $ Option
        { optionStem = q010
        , optionOrdinal = "b)"
        , optionText = Textarea "120 200 14"
        , optionKey = True
        , optionPoints = 1
        }

    d010_2 <- insert $ Option
        { optionStem = q010
        , optionOrdinal = "c)"
        , optionText = Textarea "120 200 16"
        , optionKey = False
        , optionPoints = 0
        }

    d010_3 <- insert $ Option
        { optionStem = q010
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna"
        , optionKey = False
        , optionPoints = 0
        }

    q011 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 11
               , stemText = Textarea "Când o matrice este transmisă unei metode, ce primește metoda?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k011 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "a)"
        , optionText = Textarea "Referința matricei"
        , optionKey = True
        , optionPoints = 1
        }

    d011_1 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "b)"
        , optionText = Textarea "O copie a matricei"
        , optionKey = False
        , optionPoints = 0
        }

    d011_2 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "c)"
        , optionText = Textarea "Lungimea matricei"
        , optionKey = False
        , optionPoints = 0
        }

    d011_3 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "d)"
        , optionText = Textarea "Copie a primului element"
        , optionKey = False
        , optionPoints = 0
        }

    q012 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 12
               , stemText = Textarea "Selectați instrucțiunea validă pentru a declara și inițializa o matrice."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d012_1 <- insert $ Option
        { optionStem = q012
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>int[] A = {}</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k012 <- insert $ Option
        { optionStem = q012
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>int[] A = {1, 2, 3}</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d012_2 <- insert $ Option
        { optionStem = q012
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>int[] A = (1, 2, 3)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d012_3 <- insert $ Option
        { optionStem = q012
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>int[][] A = {1,2,3}</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q013 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 13
               , stemText = Textarea [st|Având în vedere,
<code><pre>int values[ ] = {1,2,3,4,5,6,7,8,9,10};
for(int i=0;i< Y; ++i)
System.out.println(values[i]);
</pre></code>
Găsiți valoarea lui <code>value[i]</code>?|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d013_1 <- insert $ Option
        { optionStem = q013
        , optionOrdinal = "a)"
        , optionText = Textarea "10"
        , optionKey = False
        , optionPoints = 0
        }

    d013_2 <- insert $ Option
        { optionStem = q013
        , optionOrdinal = "b)"
        , optionText = Textarea "11"
        , optionKey = False
        , optionPoints = 0
        }

    d013_3 <- insert $ Option
        { optionStem = q013
        , optionOrdinal = "c)"
        , optionText = Textarea "15"
        , optionKey = False
        , optionPoints = 0
        }

    k013 <- insert $ Option
        { optionStem = q013
        , optionOrdinal = "d)"
        , optionText = Textarea "Nici una dintre cele de mai sus"
        , optionKey = True
        , optionPoints = 1
        }

    q014 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 14
               , stemText = Textarea "Matricele în java sunt:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d014_1 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "a)"
        , optionText = Textarea "Referințe la obiecte"
        , optionKey = False
        , optionPoints = 0
        }

    k014 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "b)"
        , optionText = Textarea "obiecte"
        , optionKey = True
        , optionPoints = 1
        }

    d014_2 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "c)"
        , optionText = Textarea "Tip de date primitive"
        , optionKey = False
        , optionPoints = 0
        }

    d014_3 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna"
        , optionKey = False
        , optionPoints = 0
        }

    q015 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 15
               , stemText = Textarea "Identificați definiția corectată a unui pachet."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d015_1 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "a)"
        , optionText = Textarea "Un pachet este o colecție de instrumente de editare"
        , optionKey = False
        , optionPoints = 0
        }

    d015_2 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "b)"
        , optionText = Textarea "Un pachet este o colecție de clase"
        , optionKey = False
        , optionPoints = 0
        }

    k015 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "c)"
        , optionText = Textarea "Un pachet este o colecție de clase și interfețe"
        , optionKey = True
        , optionPoints = 1
        }

    d015_3 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "d)"
        , optionText = Textarea "Un pachet este o colecție de interfețe"
        , optionKey = False
        , optionPoints = 0
        }

    q016 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 16
               , stemText = Textarea [st|Identificați restricția corectă asupra metodelor statice.
  <ol>
    <li>Ei trebuie să acceseze numai date statice.</li>
    <li>Ei pot apela doar alte metode statice.</li>
    <li>Ei nu se pot referi la <code>this</code> sau <code>super</code>.</li>
  </ol>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d016_1 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "a)"
        , optionText = Textarea "I and II"
        , optionKey = False
        , optionPoints = 0
        }

    d016_2 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "b)"
        , optionText = Textarea "II and III"
        , optionKey = False
        , optionPoints = 0
        }

    d016_3 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "c)"
        , optionText = Textarea "Only III"
        , optionKey = False
        , optionPoints = 0
        }

    k016 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "d)"
        , optionText = Textarea "I, II and III"
        , optionKey = True
        , optionPoints = 1
        }

    q017 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 17
               , stemText = Textarea "Identificați cuvântul cheie dintre următoarele care face ca o variabilă să aparțină unei clase, în loc să fie definită pentru fiecare instanță a clasei."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d017_1 <- insert $ Option
        { optionStem = q017
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>final</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k017 <- insert $ Option
        { optionStem = q017
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>static</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d017_2 <- insert $ Option
        { optionStem = q017
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>volatile</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d017_3 <- insert $ Option
        { optionStem = q017
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>abstract</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q018 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 18
               , stemText = Textarea [st|Identificați ceea ce poate accesa direct și modifica valoarea variabilei <code>res</code>.
<code><pre>
  package com.mypackage;
  public class Solution{
    private int res = 100;
  }
</pre></code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d018_1 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "a)"
        , optionText = Textarea "Orice clasa"
        , optionKey = False
        , optionPoints = 0
        }

    k018 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "b)"
        , optionText = Textarea "Doar clasa Solution"
        , optionKey = True
        , optionPoints = 1
        }

    d018_2 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "c)"
        , optionText = Textarea "Orice clasă care extinde Solution"
        , optionKey = False
        , optionPoints = 0
        }

    d018_3 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna"
        , optionKey = False
        , optionPoints = 0
        }

    q019 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 19
               , stemText = Textarea "În care dintre următoarele este definită metoda <code>toString()</code>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k019 <- insert $ Option
        { optionStem = q019
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>java.lang.Object</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d019_1 <- insert $ Option
        { optionStem = q019
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>java.lang.String</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d019_2 <- insert $ Option
        { optionStem = q019
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>java.lang.util</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d019_3 <- insert $ Option
        { optionStem = q019
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna"
        , optionKey = False
        , optionPoints = 0
        }

    q020 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 20
               , stemText = Textarea "<code>compareTo()</code> întoarce"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d020_1 <- insert $ Option
        { optionStem = q020
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>true</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d020_2 <- insert $ Option
        { optionStem = q020
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>false</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k020 <- insert $ Option
        { optionStem = q020
        , optionOrdinal = "c)"
        , optionText = Textarea "O valoare <code>int</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d020_3 <- insert $ Option
        { optionStem = q020
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna"
        , optionKey = False
        , optionPoints = 0
        }

    e001 <- insert $ Exam
        { examTest = t001
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-20) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (q001,d001_3)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,d005_2)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,d010_3)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_1)
          , (q016,k016)
          , (q017,k017)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e001
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e002 <- insert $ Exam
        { examTest = t001
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-40) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,d004_1)
          , (q005,d005_2)
          , (q006,d006_3)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,d011_1)
          , (q012,k012)
          , (q013,k013)
          , (q014,d014_1)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,d018_3)
          , (q019,d019_3)
          , (q020,d020_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e002
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e003 <- insert $ Exam
        { examTest = t001
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-45) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,d004_1)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,d011_1)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e003
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e004 <- insert $ Exam
        { examTest = t001
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-55) now
        , examEnd = pure $ addUTCTime (-15) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,d004_1)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e004
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e005 <- insert $ Exam
        { examTest = t001
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-50) now
        , examEnd = pure $ addUTCTime (-15) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e005
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e006 <- insert $ Exam
        { examTest = t001
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-3000) now
        , examEnd = pure $ addUTCTime (-2955) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,d019_1)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e006
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e007 <- insert $ Exam
        { examTest = t001
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-4000) now
        , examEnd = pure $ addUTCTime (-3950) now
        }

    forM_ [ (q001,k001)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,d010_2)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e007
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e008 <- insert $ Exam
        { examTest = t001
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-5000) now
        , examEnd = pure $ addUTCTime (-4965) now
        }

    forM_ [ (q001,k001)
          , (q002,d002_2)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,d010_2)
          , (q011,k011)
          , (q012,k012)
          , (q013,k013)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e008
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e009 <- insert $ Exam
        { examTest = t001
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-6000) now
        , examEnd = pure $ addUTCTime (-5960) now
        }

    forM_ [ (q001,k001)
          , (q002,d002_2)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,k010)
          , (q011,k011)
          , (q012,k012)
          , (q013,d013_3)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e009
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e010 <- insert $ Exam
        { examTest = t001
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-6040) now
        , examEnd = pure $ addUTCTime (-6000) now
        }

    forM_ [ (q001,d001_3)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,d010_2)
          , (q011,k011)
          , (q012,k012)
          , (q013,d013_3)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e010
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e011 <- insert $ Exam
        { examTest = t001
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-5040) now
        , examEnd = pure $ addUTCTime (-5005) now
        }

    forM_ [ (q001,d001_3)
          , (q002,k002_1)
          , (q002,k002_2)
          , (q003,k003)
          , (q004,k004)
          , (q005,k005)
          , (q006,k006)
          , (q007,k007)
          , (q008,k008)
          , (q009,k009)
          , (q010,d010_2)
          , (q011,d011_2)
          , (q012,d012_1)
          , (q013,d013_3)
          , (q014,k014)
          , (q015,d015_3)
          , (q016,k016)
          , (q017,d017_1)
          , (q018,k018)
          , (q019,k019)
          , (q020,k020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e011
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s101 <- insert $ Skill
        { skillCode = "Python 101"
        , skillName = "Bazele programării Python"
        , skillDescr = Just "Abilități de programare în Python"
        }

    t101 <- insert $ Test
        { testCode = "E201"
        , testName = "Introducere în programarea Python"
        , testDuration = 10
        , testPass = 8
        , testDescr = Just $ Textarea "Testați abilitățile de bază de programare Python"
        , testState = TestStatePublished
        }

    q101 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 1
               , stemText = Textarea "Cine a dezvoltat limbajul de programare Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d101_1 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "a)"
        , optionText = Textarea "Wick van Rossum"
        , optionKey = False
        , optionPoints = 0
        }

    d101_2 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "b)"
        , optionText = Textarea "Rasmus Lerdorf"
        , optionKey = False
        , optionPoints = 0
        }

    k101 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "c)"
        , optionText = Textarea "Guido van Rossum"
        , optionKey = True
        , optionPoints = 1
        }
        
    d101_3 <- insert $ Option
        { optionStem = q101
        , optionOrdinal = "d)"
        , optionText = Textarea "Niene Stom"
        , optionKey = False
        , optionPoints = 0
        }

    q102 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 2
               , stemText = Textarea "Ce tip de programare acceptă Python?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Select all true"
               }

    k102_1 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "a)"
        , optionText = Textarea "programare orientată pe obiecte"
        , optionKey = True
        , optionPoints = 1
        }

    k102_2 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "b)"
        , optionText = Textarea "programare structurată"
        , optionKey = True
        , optionPoints = 1
        }

    k102_3 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "c)"
        , optionText = Textarea "programare functionala"
        , optionKey = True
        , optionPoints = 1
        }
        
    d102 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "d)"
        , optionText = Textarea "programare procedurală"
        , optionKey = False
        , optionPoints = 0
        }

    q103 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 3
               , stemText = Textarea "Python ține cont de majuscule și minuscule atunci când se ocupă de identificatori?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d103_1 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "a)"
        , optionText = Textarea "nu"
        , optionKey = False
        , optionPoints = 0
        }

    k103 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "b)"
        , optionText = Textarea "da"
        , optionKey = True
        , optionPoints = 1
        }

    d103_2 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "c)"
        , optionText = Textarea "dependent de mașină"
        , optionKey = False
        , optionPoints = 0
        }
        
    d103_3 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "d)"
        , optionText = Textarea "niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q104 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 4
               , stemText = Textarea "Care dintre următoarele este extensia corectă a fișierului Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d104_1 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>.python</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d104_2 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>.pl</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k104 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>.py</code>"
        , optionKey = True
        , optionPoints = 1
        }
        
    d104_3 <- insert $ Option
        { optionStem = q104
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>.p</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q105 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 5
               , stemText = Textarea "Codul Python este compilat sau interpretat?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k105 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "a)"
        , optionText = Textarea "Codul Python este atât compilat, cât și interpretat"
        , optionKey = True
        , optionPoints = 1
        }

    d105_1 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "b)"
        , optionText = Textarea "Codul Python nu este nici compilat, nici interpretat"
        , optionKey = False
        , optionPoints = 0
        }

    d105_2 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "c)"
        , optionText = Textarea "Codul Python este doar compilat"
        , optionKey = False
        , optionPoints = 0
        }
        
    d105_3 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "d)"
        , optionText = Textarea "Codul Python este doar interpretat"
        , optionKey = False
        , optionPoints = 0
        }

    q106 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 6
               , stemText = Textarea "Toate cuvintele cheie din Python sunt în&nbsp;_________"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d106_1 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "a)"
        , optionText = Textarea "Cu majuscule"
        , optionKey = False
        , optionPoints = 0
        }

    d106_2 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "b)"
        , optionText = Textarea "minuscule"
        , optionKey = False
        , optionPoints = 0
        }

    d106_3 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "c)"
        , optionText = Textarea "MAJUSCULE"
        , optionKey = False
        , optionPoints = 0
        }
        
    k106 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = True
        , optionPoints = 1
        }

    q107 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 7
               , stemText = Textarea [st|Care va fi valoarea următoarei expresii Python?
<code><pre>
  4 + 3 % 5
</pre></code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k107 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "a)"
        , optionText = Textarea "7"
        , optionKey = True
        , optionPoints = 1
        }

    d107_1 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "b)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    d107_2 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "c)"
        , optionText = Textarea "4"
        , optionKey = False
        , optionPoints = 0
        }
        
    d107_3 <- insert $ Option
        { optionStem = q107
        , optionOrdinal = "d)"
        , optionText = Textarea "1"
        , optionKey = False
        , optionPoints = 0
        }

    q108 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 8
               , stemText = Textarea "Care dintre următoarele este folosită pentru a defini un bloc de cod în limbajul Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k108 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "a)"
        , optionText = Textarea "Indentare"
        , optionKey = True
        , optionPoints = 1
        }

    d108_1 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "b)"
        , optionText = Textarea "Cheie"
        , optionKey = False
        , optionPoints = 0
        }

    d108_2 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "c)"
        , optionText = Textarea "Paranteze"
        , optionKey = False
        , optionPoints = 0
        }
        
    d108_3 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "d)"
        , optionText = Textarea "Toate cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q109 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 9
               , stemText = Textarea "Ce cuvânt cheie este folosit pentru funcție în limbajul Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d109_1 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>Function</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k109 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>def</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d109_2 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>Fun</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    d109_3 <- insert $ Option
        { optionStem = q109
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>Define</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q110 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 10
               , stemText = Textarea "Care dintre următoarele caractere este folosit pentru a oferi comentarii pe o singură linie în Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d110_1 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>//</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k110 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>#</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d110_2 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>!</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    d110_3 <- insert $ Option
        { optionStem = q110
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>/*</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q111 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 11
               , stemText = Textarea [st|Care va fi rezultatul următorului cod Python?
<pre><code>
  i = 1
  while True:
      if i%3 == 0:
          break
      print(i)

      i + = 1
</cpde></pre>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d111_1 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>1 2 3</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k111 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "b)"
        , optionText = Textarea "eroare"
        , optionKey = True
        , optionPoints = 1
        }

    d111_2 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>1 2</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    d111_3 <- insert $ Option
        { optionStem = q111
        , optionOrdinal = "d)"
        , optionText = Textarea "niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q112 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 12
               , stemText = Textarea "Care dintre următoarele funcții ne poate ajuta să găsim versiunea de python la care lucrăm în prezent?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d112_1 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>sys.version(1)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d112_2 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>sys.version(0)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d112_3 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>sys.version()</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    k112 <- insert $ Option
        { optionStem = q112
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>sys.version</code>"
        , optionKey = True
        , optionPoints = 1
        }

    e101 <- insert $ Exam
        { examTest = t101
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-5040) now
        , examEnd = pure $ addUTCTime (-5005) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,d103_2)
          , (q104,k104)
          , (q105,k105)
          , (q106,d106_3)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_2)
          , (q111,k111)
          , (q112,d112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e101
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e102 <- insert $ Exam
        { examTest = t101
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-45) now
        , examEnd = pure $ addUTCTime (-35) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,d103_2)
          , (q104,k104)
          , (q105,k105)
          , (q106,d106_3)
          , (q107,k107)
          , (q108,d108_2)
          , (q109,k109)
          , (q110,d110_2)
          , (q111,k111)
          , (q112,d112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e102
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e103 <- insert $ Exam
        { examTest = t101
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-55) now
        , examEnd = pure $ addUTCTime (-47) now
        }

    forM_ [ (q101,d101_1)
          , (q102,k102_1)
          , (q102,d102)
          , (q102,k102_3)
          , (q103,d103_2)
          , (q104,k104)
          , (q105,k105)
          , (q106,d106_3)
          , (q107,k107)
          , (q108,d108_2)
          , (q109,k109)
          , (q110,d110_2)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e103
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e104 <- insert $ Exam
        { examTest = t101
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-65) now
        , examEnd = pure $ addUTCTime (-56) now
        }

    forM_ [ (q101,d101_1)
          , (q102,k102_1)
          , (q102,d102)
          , (q102,k102_3)
          , (q103,d103_2)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,d108_2)
          , (q109,k109)
          , (q110,d110_2)
          , (q111,d111_1)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e104
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e105 <- insert $ Exam
        { examTest = t101
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-80) now
        , examEnd = pure $ addUTCTime (-50) now
        }

    forM_ [ (q101,d101_1)
          , (q102,k102_1)
          , (q102,d102)
          , (q102,k102_3)
          , (q103,d103_2)
          , (q104,d104_1)
          , (q105,d105_2)
          , (q106,d106_3)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_2)
          , (q111,d111_1)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e105
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e106 <- insert $ Exam
        { examTest = t101
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-90) now
        , examEnd = pure $ addUTCTime (-80) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,k103)
          , (q104,d104_1)
          , (q105,k105)
          , (q106,d106_3)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e106
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e107 <- insert $ Exam
        { examTest = t101
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-100) now
        , examEnd = pure $ addUTCTime (-95) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,k103)
          , (q104,d104_1)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e107
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e108 <- insert $ Exam
        { examTest = t101
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-103) now
        , examEnd = pure $ addUTCTime (-96) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,k103)
          , (q104,d104_1)
          , (q105,d105_2)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,k110)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e108
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e109 <- insert $ Exam
        { examTest = t101
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-203) now
        , examEnd = pure $ addUTCTime (-194) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_1)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e109
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e110 <- insert $ Exam
        { examTest = t101
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-302) now
        , examEnd = pure $ addUTCTime (-296) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_3)
          , (q103,k103)
          , (q104,k104)
          , (q105,k105)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_1)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e110
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e111 <- insert $ Exam
        { examTest = t101
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-312) now
        , examEnd = pure $ addUTCTime (-302) now
        }

    forM_ [ (q101,k101)
          , (q102,k102_1)
          , (q102,k102_2)
          , (q102,k102_3)
          , (q102,d102)
          , (q103,k103)
          , (q104,k104)
          , (q105,d105_3)
          , (q106,k106)
          , (q107,k107)
          , (q108,k108)
          , (q109,k109)
          , (q110,d110_1)
          , (q111,k111)
          , (q112,k112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e111
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s201 <- insert $ Skill
        { skillCode = "Inginerie chimică de bază"
        , skillName = "Inginerie chimică de bază"
        , skillDescr = Just "Abilități pentru inginerie chimică de bază"
        }

    t201 <- insert $ Test
        { testCode = "E202"
        , testName = "Inginerie Chimica"
        , testDuration = 20
        , testPass = 8
        , testDescr = Just $ Textarea "Testează abilitățile de bază de inginerie chimică"
        , testState = TestStatePublished
        }

    q201 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 1
               , stemText = Textarea "Care este unitatea de greutate specifică?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k201 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "a)"
        , optionText = Textarea "Fără dimensiuni"
        , optionKey = True
        , optionPoints = 1
        }

    d201_1 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "b)"
        , optionText = Textarea "m/s<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d201_2 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "c)"
        , optionText = Textarea "N/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    d201_3 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "d)"
        , optionText = Textarea "Kg/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    q202 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 2
               , stemText = Textarea "Care dintre următoarele are același număr de moli ca în 398 de grame de CuSO<sub>4</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d202_1 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "a)"
        , optionText = Textarea "35 de grame de azot"
        , optionKey = False
        , optionPoints = 0
        }

    d202_2 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "b)"
        , optionText = Textarea "58,5 grame de clorură de sodiu"
        , optionKey = False
        , optionPoints = 0
        }

    d202_3 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "c)"
        , optionText = Textarea "2 grame de hidrogen"
        , optionKey = False
        , optionPoints = 0
        }
        
    k202 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "d)"
        , optionText = Textarea "40 de grame de oxigen"
        , optionKey = True
        , optionPoints = 1
        }

    q203 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 3
               , stemText = Textarea "Care este greutatea specifică a 5 Kg de apă ocupată în 10 m<sup>3</sup> față de 500 g/m<sup>3</sup>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d203_1 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "a)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    d203_2 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "b)"
        , optionText = Textarea "5"
        , optionKey = False
        , optionPoints = 0
        }

    d203_3 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "c)"
        , optionText = Textarea "0,5"
        , optionKey = False
        , optionPoints = 0
        }
        
    k203 <- insert $ Option
        { optionStem = q203
        , optionOrdinal = "d)"
        , optionText = Textarea "1"
        , optionKey = True
        , optionPoints = 1
        }

    q204 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 4
               , stemText = Textarea "Care este unitatea de unitate a fracției molare?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d204_1 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "a)"
        , optionText = Textarea "N/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d204_2 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "b)"
        , optionText = Textarea "m<sup>-2</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    d204_3 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "c)"
        , optionText = Textarea "Kg/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    k204 <- insert $ Option
        { optionStem = q204
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = True
        , optionPoints = 1
        }

    q205 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 5
               , stemText = Textarea "Care este greutatea a 10 moli dintr-un amestec cu compoziție 15% O<sub>2</sub>, 25% SO<sub>2</sub>, 30% COCl<sub>2</sub>, 25% SO<sub>3</sub> și 5% N<sub>2</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d205_1 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "a)"
        , optionText = Textarea "564"
        , optionKey = False
        , optionPoints = 0
        }

    d205_2 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "b)"
        , optionText = Textarea "475"
        , optionKey = False
        , optionPoints = 0
        }

    d205_3 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "c)"
        , optionText = Textarea "867"
        , optionKey = False
        , optionPoints = 0
        }
        
    k205 <- insert $ Option
        { optionStem = q205
        , optionOrdinal = "d)"
        , optionText = Textarea "719"
        , optionKey = True
        , optionPoints = 1
        }

    q206 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 6
               , stemText = Textarea "Care este 100<sup>o</sup>C în grade Fahrenheit?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d206_1 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "a)"
        , optionText = Textarea "100<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    k206 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "b)"
        , optionText = Textarea "212<sup>o</sup>F"
        , optionKey = True
        , optionPoints = 1
        }

    d206_2 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "c)"
        , optionText = Textarea "460<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }
        
    d206_3 <- insert $ Option
        { optionStem = q206
        , optionOrdinal = "d)"
        , optionText = Textarea "0<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    q207 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 7
               , stemText = Textarea "Care este presiunea de 1900 Torr în bar?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d207_1 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "a)"
        , optionText = Textarea "2,46"
        , optionKey = False
        , optionPoints = 0
        }

    d207_2 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "b)"
        , optionText = Textarea "2,87"
        , optionKey = False
        , optionPoints = 0
        }

    k207 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "c)"
        , optionText = Textarea "2,40"
        , optionKey = True
        , optionPoints = 1
        }
        
    d207_3 <- insert $ Option
        { optionStem = q207
        , optionOrdinal = "d)"
        , optionText = Textarea "2,68"
        , optionKey = False
        , optionPoints = 0
        }

    q208 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 8
               , stemText = Textarea "Care dintre următoarele nu este un dispozitiv de măsurare a presiunii?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k208 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "a)"
        , optionText = Textarea "Galvanometru"
        , optionKey = True
        , optionPoints = 1
        }

    d208_1 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "b)"
        , optionText = Textarea "Manometru"
        , optionKey = False
        , optionPoints = 0
        }

    d208_2 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "c)"
        , optionText = Textarea "Barometru"
        , optionKey = False
        , optionPoints = 0
        }
        
    d208_3 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q209 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 9
               , stemText = Textarea "Care dintre următoarele este folosită pentru măsurarea presiunii numai a lichidului?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d209_1 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "a)"
        , optionText = Textarea "Manometru diferențial"
        , optionKey = False
        , optionPoints = 0
        }

    d209_2 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "b)"
        , optionText = Textarea "Manometru"
        , optionKey = False
        , optionPoints = 0
        }

    k209 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "c)"
        , optionText = Textarea "Piezometru"
        , optionKey = True
        , optionPoints = 1
        }
        
    d209_3 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q210 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 10
               , stemText = Textarea "Care dintre următoarele este o funcție de stare?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k210 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "a)"
        , optionText = Textarea "Entropia"
        , optionKey = True
        , optionPoints = 1
        }

    d210_1 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "b)"
        , optionText = Textarea "Căldura"
        , optionKey = False
        , optionPoints = 0
        }

    d210_2 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "c)"
        , optionText = Textarea "Lucru"
        , optionKey = False
        , optionPoints = 0
        }
        
    d210_3 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    q211 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 11
               , stemText = Textarea "Apa care fierbe într-un recipient este un exemplu pentru care dintre următoarele?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d211_1 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "a)"
        , optionText = Textarea "Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }

    d211_2 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "b)"
        , optionText = Textarea "Batch"
        , optionKey = False
        , optionPoints = 0
        }

    d211_3 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "c)"
        , optionText = Textarea "Batch și Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }
        
    k211 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre acestea"
        , optionKey = True
        , optionPoints = 1
        }

    q212 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 12
               , stemText = Textarea "Care dintre următoarele este adevărată cu privire la reactivii de limitare?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d212_1 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "a)"
        , optionText = Textarea "Consumă parțial"
        , optionKey = False
        , optionPoints = 0
        }

    d212_2 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "b)"
        , optionText = Textarea "Nu reactioneaza"
        , optionKey = False
        , optionPoints = 0
        }

    k212 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "c)"
        , optionText = Textarea "Consumă complet"
        , optionKey = True
        , optionPoints = 1
        }
        
    d212_3 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "d)"
        , optionText = Textarea "Niciuna dintre cele menționate"
        , optionKey = False
        , optionPoints = 0
        }

    e201 <- insert $ Exam
        { examTest = t201
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-3012) now
        , examEnd = pure $ addUTCTime (-3002) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e201
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e202 <- insert $ Exam
        { examTest = t201
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-4015) now
        , examEnd = pure $ addUTCTime (-4005) now
        }

    forM_ [ (q201,d201_1)
          , (q202,d201_2)
          , (q203,d203_3)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_1)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e202
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e203 <- insert $ Exam
        { examTest = t201
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-2015) now
        , examEnd = pure $ addUTCTime (-2005) now
        }

    forM_ [ (q201,d201_1)
          , (q202,d201_2)
          , (q203,k203)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,k207)
          , (q208,d208_1)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e203
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e204 <- insert $ Exam
        { examTest = t201
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-1013) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,d207_3)
          , (q208,k208)
          , (q209,k209)
          , (q210,k210)
          , (q211,k211)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e204
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e205 <- insert $ Exam
        { examTest = t201
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-1020) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,d203_2)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,d207_3)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_3)
          , (q211,k211)
          , (q212,d212_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e205
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e206 <- insert $ Exam
        { examTest = t201
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-2021) now
        , examEnd = pure $ addUTCTime (-2002) now
        }

    forM_ [ (q201,k201)
          , (q202,k202)
          , (q203,k203)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,k207)
          , (q208,k208)
          , (q209,k209)
          , (q210,d210_3)
          , (q211,k211)
          , (q212,d212_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e206
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e207 <- insert $ Exam
        { examTest = t201
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-5021) now
        , examEnd = pure $ addUTCTime (-5001) now
        }

    forM_ [ (q201,d201_2)
          , (q202,k202)
          , (q203,d203_1)
          , (q204,k204)
          , (q205,d205_3)
          , (q206,k206)
          , (q207,d207_1)
          , (q208,k208)
          , (q209,d209_2)
          , (q210,k210)
          , (q211,d211_3)
          , (q212,k212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e207
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    s301 <- insert $ Skill
                { skillCode = "SQL"
                , skillName = "SQL"
                , skillDescr = Just "Abilități de a scrie SQL"
                }

    t301 <- insert $ Test
        { testCode = "E301"
        , testName = "SQL"
        , testDuration = 30
        , testPass = 9
        , testDescr = Just $ Textarea "Programare SQL"
        , testState = TestStatePublished
        }

    q301 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 1
               , stemText = Textarea "Care este forma completă a SQL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d301_1 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "a."
        , optionText = Textarea "Structured Query List"
        , optionKey = False
        , optionPoints = 0
        }

    k301 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "b."
        , optionText = Textarea "Structure Query Language"
        , optionKey = True
        , optionPoints = 1
        }

    d301_2 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "c."
        , optionText = Textarea "Sample Query Language"
        , optionKey = False
        , optionPoints = 0
        }

    d301_3 <- insert $ Option
        { optionStem = q301
        , optionOrdinal = "d."
        , optionText = Textarea "Niciuna dintre acestea"
        , optionKey = False
        , optionPoints = 0
        }

    q302 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 2
               , stemText = Textarea "Care dintre următoarele nu este un tip SQL valid?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d302_1 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "a."
        , optionText = Textarea "FLOAT"
        , optionKey = False
        , optionPoints = 0
        }

    d302_2 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "b."
        , optionText = Textarea "NUMERIC"
        , optionKey = False
        , optionPoints = 0
        }

    k302 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "c."
        , optionText = Textarea "DECIMAL"
        , optionKey = True
        , optionPoints = 1
        }

    d302_3 <- insert $ Option
        { optionStem = q302
        , optionOrdinal = "d."
        , optionText = Textarea "CHARACTER"
        , optionKey = False
        , optionPoints = 0
        }

    q303 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 3
               , stemText = Textarea "Care dintre următoarele nu este o comandă DDL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d303_1 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "a."
        , optionText = Textarea "TRUNCATE"
        , optionKey = False
        , optionPoints = 0
        }

    d303_2 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "b."
        , optionText = Textarea "ALTER"
        , optionKey = False
        , optionPoints = 0
        }

    d303_3 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "c."
        , optionText = Textarea "CREATE"
        , optionKey = False
        , optionPoints = 0
        }

    k303 <- insert $ Option
        { optionStem = q303
        , optionOrdinal = "d."
        , optionText = Textarea "UPDATE"
        , optionKey = True
        , optionPoints = 1
        }

    q304 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 4
               , stemText = Textarea "Care dintre următoarele sunt comenzi TCL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k304 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "a."
        , optionText = Textarea "COMMIT și ROLLBACK"
        , optionKey = True
        , optionPoints = 1
        }

    d304_1 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "b."
        , optionText = Textarea "UPDATE și TRUNCATE"
        , optionKey = False
        , optionPoints = 0
        }

    d304_2 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "c."
        , optionText = Textarea "SELECT și INSERT"
        , optionKey = False
        , optionPoints = 0
        }

    d304_3 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "d."
        , optionText = Textarea "GRANT și REVOKE"
        , optionKey = False
        , optionPoints = 0
        }

    q305 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 5
               , stemText = Textarea "Ce instrucțiune este folosită pentru a șterge toate rândurile dintr-un tabel fără a avea acțiunea înregistrată?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d305_1 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "a."
        , optionText = Textarea "DELETE"
        , optionKey = False
        , optionPoints = 0
        }

    d305_2 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "b."
        , optionText = Textarea "REMOVE"
        , optionKey = False
        , optionPoints = 0
        }

    d305_3 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "c."
        , optionText = Textarea "DROP"
        , optionKey = False
        , optionPoints = 0
        }

    k305 <- insert $ Option
        { optionStem = q305
        , optionOrdinal = "d."
        , optionText = Textarea "TRUNCATE"
        , optionKey = True
        , optionPoints = 1
        }

    q306 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 6
               , stemText = Textarea "Vizualizările SQL sunt cunoscute și ca"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d306_1 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "a."
        , optionText = Textarea "Tabele simple"
        , optionKey = False
        , optionPoints = 0
        }

    k306 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "b."
        , optionText = Textarea "Tabele virtuale"
        , optionKey = True
        , optionPoints = 1
        }

    d306_2 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "c."
        , optionText = Textarea "Tabele complexe"
        , optionKey = False
        , optionPoints = 0
        }

    d306_3 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "d."
        , optionText = Textarea "Tabelele reale"
        , optionKey = False
        , optionPoints = 0
        }

    q307 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 7
               , stemText = Textarea "Câte chei primare pot avea într-un tabel?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    k307 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "a."
        , optionText = Textarea "Doar 1"
        , optionKey = True
        , optionPoints = 1
        }

    d307_1 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "b."
        , optionText = Textarea "Doar 2"
        , optionKey = False
        , optionPoints = 0
        }

    d307_2 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "c."
        , optionText = Textarea "Depinde de numărul de coloane"
        , optionKey = False
        , optionPoints = 0
        }

    d307_3 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "d."
        , optionText = Textarea "Depinde de DBA"
        , optionKey = False
        , optionPoints = 0
        }

    q308 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 8
               , stemText = Textarea "Ce tip de date poate stoca date nestructurate într-o coloană?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d308_1 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "a."
        , optionText = Textarea "CHAR"
        , optionKey = False
        , optionPoints = 0
        }

    k308 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "b."
        , optionText = Textarea "RAW"
        , optionKey = True
        , optionPoints = 1
        }

    d308_2 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "c."
        , optionText = Textarea "NUMERIC"
        , optionKey = False
        , optionPoints = 0
        }

    d308_3 <- insert $ Option
        { optionStem = q308
        , optionOrdinal = "d."
        , optionText = Textarea "VARCHAR"
        , optionKey = False
        , optionPoints = 0
        }

    q309 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 9
               , stemText = Textarea "Care dintre următoarele nu este constrângere în SQL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d309_1 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "a."
        , optionText = Textarea "Primary Key"
        , optionKey = False
        , optionPoints = 0
        }

    d309_2 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "b."
        , optionText = Textarea "Not Null"
        , optionKey = False
        , optionPoints = 0
        }

    d309_3 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "c."
        , optionText = Textarea "Check"
        , optionKey = False
        , optionPoints = 0
        }

    k309 <- insert $ Option
        { optionStem = q309
        , optionOrdinal = "d."
        , optionText = Textarea "Union"
        , optionKey = True
        , optionPoints = 1
        }

    q310 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 10
               , stemText = Textarea "Care dintre următoarele nu este o funcție agregată validă?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d310_1 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "a."
        , optionText = Textarea "COUNT"
        , optionKey = False
        , optionPoints = 0
        }

    k310 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "b."
        , optionText = Textarea "COMPUTE"
        , optionKey = True
        , optionPoints = 1
        }

    d310_2 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "c."
        , optionText = Textarea "SUM"
        , optionKey = False
        , optionPoints = 0
        }

    d310_3 <- insert $ Option
        { optionStem = q310
        , optionOrdinal = "d."
        , optionText = Textarea "MAX"
        , optionKey = False
        , optionPoints = 0
        }

    q311 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 11
               , stemText = Textarea "Ce comandă de manipulare a datelor este utilizată pentru a combina înregistrările dintr-unul sau mai multe tabele?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d311_1 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "a."
        , optionText = Textarea "SELECT"
        , optionKey = False
        , optionPoints = 0
        }

    d311_2 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "b."
        , optionText = Textarea "PROJECT"
        , optionKey = False
        , optionPoints = 0
        }

    k311 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "c."
        , optionText = Textarea "JOIN"
        , optionKey = True
        , optionPoints = 1
        }

    d311_3 <- insert $ Option
        { optionStem = q311
        , optionOrdinal = "d."
        , optionText = Textarea "PRODUCT"
        , optionKey = False
        , optionPoints = 0
        }

    q312 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 12
               , stemText = Textarea "Ce operator este folosit pentru a compara o valoare cu o listă specificată de valori?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Selectați unul"
               }

    d312_1 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "a."
        , optionText = Textarea "ANY"
        , optionKey = False
        , optionPoints = 0
        }

    d312_2 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "b."
        , optionText = Textarea "BETWEEN"
        , optionKey = False
        , optionPoints = 0
        }

    d312_3 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "c."
        , optionText = Textarea "ALL"
        , optionKey = False
        , optionPoints = 0
        }

    k312 <- insert $ Option
        { optionStem = q312
        , optionOrdinal = "d."
        , optionText = Textarea "IN"
        , optionKey = True
        , optionPoints = 1
        }

    e301 <- insert $ Exam
        { examTest = t301
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-4024) now
        , examEnd = pure $ addUTCTime (-4001) now
        }

    forM_ [ (q301,d301_2)
          , (q302,k302)
          , (q303,d303_1)
          , (q304,k304)
          , (q305,d305_3)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,d309_2)
          , (q310,k310)
          , (q311,d311_3)
          , (q312,k312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e301
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e302 <- insert $ Exam
        { examTest = t301
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-3024) now
        , examEnd = pure $ addUTCTime (-3001) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,d303_1)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e302
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e303 <- insert $ Exam
        { examTest = t301
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-2024) now
        , examEnd = pure $ addUTCTime (-2001) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,d304_2)
          , (q305,k305)
          , (q306,d306_2)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e303
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e304 <- insert $ Exam
        { examTest = t301
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-1026) now
        , examEnd = pure $ addUTCTime (-1002) now
        }

    forM_ [ (q301,d301_1)
          , (q302,k302)
          , (q303,k303)
          , (q304,d304_2)
          , (q305,k305)
          , (q306,d306_2)
          , (q307,d307_1)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,d312_3)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e304
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e305 <- insert $ Exam
        { examTest = t301
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-926) now
        , examEnd = pure $ addUTCTime (-900) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,k303)
          , (q304,d304_2)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e305
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e306 <- insert $ Exam
        { examTest = t301
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-823) now
        , examEnd = pure $ addUTCTime (-800) now
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
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e306
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e307 <- insert $ Exam
        { examTest = t301
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-720) now
        , examEnd = pure $ addUTCTime (-700) now
        }

    forM_ [ (q301,d301_3)
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
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e307
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e308 <- insert $ Exam
        { examTest = t301
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-820) now
        , examEnd = pure $ addUTCTime (-803) now
        }

    forM_ [ (q301,d301_3)
          , (q302,k302)
          , (q303,d303_2)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,k312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e308
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e309 <- insert $ Exam
        { examTest = t301
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-920) now
        , examEnd = pure $ addUTCTime (-904) now
        }

    forM_ [ (q301,d301_3)
          , (q302,k302)
          , (q303,d303_2)
          , (q304,k304)
          , (q305,k305)
          , (q306,k306)
          , (q307,k307)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,d312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e309
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e310 <- insert $ Exam
        { examTest = t301
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-1020) now
        , examEnd = pure $ addUTCTime (-1005) now
        }

    forM_ [ (q301,d301_3)
          , (q302,k302)
          , (q303,d303_2)
          , (q304,k304)
          , (q305,k305)
          , (q306,d306_2)
          , (q307,k307)
          , (q308,k308)
          , (q309,k309)
          , (q310,k310)
          , (q311,k311)
          , (q312,d312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e310
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    e311 <- insert $ Exam
        { examTest = t301
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-1112) now
        , examEnd = pure $ addUTCTime (-1100) now
        }

    forM_ [ (q301,k301)
          , (q302,k302)
          , (q303,d303_2)
          , (q304,k304)
          , (q305,k305)
          , (q306,d306_2)
          , (q307,k307)
          , (q308,k308)
          , (q309,d309_1)
          , (q310,k310)
          , (q311,k311)
          , (q312,d312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = e311
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }
