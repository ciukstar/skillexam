{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoDataFR (populateFR) where

import ClassyPrelude.Yesod (ReaderT, forM_, Textarea (Textarea))
import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.ByteString as BS (readFile)

import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay), addUTCTime)

import Database.Persist ( PersistStoreWrite(insert, insert_) )
import Database.Persist.Sql (SqlBackend)

import Model
    ( Skill(Skill, skillCode, skillName, skillDescr)
    , Candidate
      ( Candidate, candidateFamilyName, candidateGivenName, candidateAdditionalName
      , candidateBday, candidateUser
      )
    , Photo (Photo, photoCandidate, photoPhoto, photoMime)
    , Test (Test, testCode, testName, testDuration, testPass, testDescr, testState)
    , TestState (TestStatePublished)
    , Stem (Stem, stemTest, stemSkill, stemOrdinal, stemText, stemType, stemInstruc)
    , StemType (SingleRespose, MultiResponse)
    , Option (Option, optionStem, optionOrdinal, optionText, optionKey, optionPoints)
    , Answer (Answer, answerExam, answerStem, answerOption, answerTime)
    , Exam (Exam, examTest, examCandidate, examAttempt, examStart, examEnd, examStatus)
    , User
      ( User, userEmail, userPassword, userName, userAdmin, userAuthType, userVerkey
      , userVerified, userSuper
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    , AuthenticationType (UserAuthTypePassword), ExamStatus (ExamStatusCompleted)
    )
    
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)


populateFR :: MonadIO m => ReaderT SqlBackend m ()
populateFR = do
    (now,today) <- liftIO $ getCurrentTime >>= \x -> return (x,utctDay x)

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "lmartin"
    uid1 <- insert $ User { userEmail = "lmartin@xmailx.fr"
                          , userPassword = Just pass1
                          , userName = Just "Léo Martin"
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

    pass2 <- liftIO $ saltPass "jbernard"
    uid2 <- insert $ User { userEmail = "jbernard@xmailx.fr"
                          , userPassword = Just pass2
                          , userName = Just "Jade Bernard"
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

    pass3 <- liftIO $ saltPass "grthomas"
    uid3 <- insert $ User { userEmail = "grthomas@xmailx.fr"
                          , userPassword = Just pass3
                          , userName = Just "Gabriel Raphaël Thomas"
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

    pass4 <- liftIO $ saltPass "lerobert"
    uid4 <- insert $ User { userEmail = "lerobert@xmailx.fr"
                          , userPassword = Just pass4
                          , userName = Just "Louise Emma Robert"
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
               { candidateFamilyName = "Martin"
               , candidateGivenName = "Léo"
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
               { candidateFamilyName = "Bernard"
               , candidateGivenName = "Jade"
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
               { candidateFamilyName = "Thomas"
               , candidateGivenName = "Gabriel"
               , candidateAdditionalName = Just "Raphaël"
               , candidateBday = Just $ addGregorianYearsClip (-21) today
               , candidateUser = Just uid3
               }
    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c003
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c004 <- insert $ Candidate
               { candidateFamilyName = "Robert"
               , candidateGivenName = "Louise"
               , candidateAdditionalName = Just "Emma"
               , candidateBday = Just $ addGregorianYearsClip (-30) today
               , candidateUser = Just uid4
               }
    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c004
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c005 <- insert $ Candidate
               { candidateFamilyName = "Richard"
               , candidateGivenName = "Arthur"
               , candidateAdditionalName = Just "Louis"
               , candidateBday = Just $ addGregorianYearsClip (-32) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c005
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c006 <- insert $ Candidate
               { candidateFamilyName = "Durand"
               , candidateGivenName = "Jules"
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
               { candidateFamilyName = "Dubois"
               , candidateGivenName = "Alice"
               , candidateAdditionalName = Just "Ambre"
               , candidateBday = Just $ addGregorianYearsClip (-35) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c007
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c008 <- insert $ Candidate
               { candidateFamilyName = "Moreau"
               , candidateGivenName = "Lina"
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
               { candidateFamilyName = "Laurent"
               , candidateGivenName = "Adam"
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
               { candidateFamilyName = "Simon"
               , candidateGivenName = "Maël"
               , candidateAdditionalName = Just "Lucas"
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_10.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c010
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }

    c011 <- insert $ Candidate
               { candidateFamilyName = "Michel"
               , candidateGivenName = "Rose"
               , candidateAdditionalName = Just "Chloé"
               , candidateBday = Just $ addGregorianYearsClip (-31) today
               , candidateUser = Nothing
               }
    liftIO (BS.readFile "demo/user_11.avif") >>= \bs ->
        insert_ Photo { photoCandidate = c011
                      , photoPhoto = bs
                      , photoMime = "image/avif"
                      }
                     
    s001 <- insert $ Skill
        { skillCode = "Jakarta EE"
        , skillName = "Java Enterprise Edition"
        , skillDescr = Just "Compétences en programmation en Java Enterprise Edition"
        }

    t001 <- insert $ Test
        { testCode = "E101"
        , testName = "Principes de base de la programmation Java"
        , testDuration = 120
        , testPass = 25
        , testDescr = Just $ Textarea "Testez les compétences de base en programmation Java"
        , testState = TestStatePublished
        }

    q001 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 1
               , stemText = Textarea "Qui a inventé la programmation Java ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quelle affirmation est vraie à propos de Java ?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Sélectionnez toutes les bonnes"
               }

    d002_1 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "a)"
        , optionText = Textarea "Java est un langage de programmation dépendant de la séquence"
        , optionKey = False
        , optionPoints = 0
        }

    d002_2 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "b)"
        , optionText = Textarea "Java est un langage de programmation dépendant du code"
        , optionKey = False
        , optionPoints = 0
        }

    k002_1 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "c)"
        , optionText = Textarea "Java est un langage de programmation dépendant de la plate-forme"
        , optionKey = True
        , optionPoints = 2
        }

    k002_2 <- insert $ Option
        { optionStem = q002
        , optionOrdinal = "d)"
        , optionText = Textarea "Java est un langage de programmation indépendant de la plate-forme"
        , optionKey = True
        , optionPoints = 5
        }

    q003 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 3
               , stemText = Textarea "Quel composant est utilisé pour compiler, déboguer et exécuter les programmes Java ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Nombre de types de données primitifs en Java sont ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quelle est la taille de float et double en java ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k005 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "a)"
        , optionText = Textarea "32 et 64"
        , optionKey = True
        , optionPoints = 1
        }

    d005_1 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "b)"
        , optionText = Textarea "32 et 32"
        , optionKey = False
        , optionPoints = 0
        }

    d005_2 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "c)"
        , optionText = Textarea "64 et 64"
        , optionKey = False
        , optionPoints = 0
        }

    d005_3 <- insert $ Option
        { optionStem = q005
        , optionOrdinal = "d)"
        , optionText = Textarea "64 et 32"
        , optionKey = False
        , optionPoints = 0
        }

    q006 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 6
               , stemText = Textarea "La conversion automatique des types est possible dans lequel des cas possibles ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d006_1 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>Byte</code> en <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    k006 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>Int</code> en <code>long</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d006_2 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>Long</code> en <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    d006_3 <- insert $ Option
        { optionStem = q006
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>Short</code> en <code>int</code>"
        , optionKey = False
        , optionPoints = 0
        }

    q007 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 7
               , stemText = Textarea [st|Trouvez la sortie du code suivant.
<code>
<pre>
  int Integer = 24;
  char String = ‘I’;
  System.out.print(Integer);
  System.out.print(String);
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d007_1 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "a)"
        , optionText = Textarea "Erreur de compilation"
        , optionKey = False
        , optionPoints = 0
        }

    d007_2 <- insert $ Option
        { optionStem = q007
        , optionOrdinal = "b)"
        , optionText = Textarea "Lève une exception"
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
               , stemText = Textarea [st|Trouvez la sortie du programme suivant.
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
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Erreur de compilation"
        , optionKey = True
        , optionPoints = 1
        }

    d008_3 <- insert $ Option
        { optionStem = q008
        , optionOrdinal = "d)"
        , optionText = Textarea "Exception"
        , optionKey = False
        , optionPoints = 0
        }

    q009 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 9
               , stemText = Textarea "Sélectionnez la déclaration valide."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea [st|Trouvez la sortie du programme suivant.
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
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Aucune"
        , optionKey = False
        , optionPoints = 0
        }

    q011 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 11
               , stemText = Textarea "Lorsqu'un tableau est passé à une méthode, que reçoit la méthode ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k011 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "a)"
        , optionText = Textarea "La référence du tableau"
        , optionKey = True
        , optionPoints = 1
        }

    d011_1 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "b)"
        , optionText = Textarea "Une copie du tableau"
        , optionKey = False
        , optionPoints = 0
        }

    d011_2 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "c)"
        , optionText = Textarea "Longueur du tableau"
        , optionKey = False
        , optionPoints = 0
        }

    d011_3 <- insert $ Option
        { optionStem = q011
        , optionOrdinal = "d)"
        , optionText = Textarea "Copie du premier élément"
        , optionKey = False
        , optionPoints = 0
        }

    q012 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 12
               , stemText = Textarea "Sélectionnez l'instruction valide pour déclarer et initialiser un tableau."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea [st|Étant donné,
<code><pre>int values[ ] = {1,2,3,4,5,6,7,8,9,10};
for(int i=0;i< Y; ++i)
System.out.println(values[i]);
</pre></code>
Trouver la valeur de <code>value[i]</code> ?|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Aucune de ces réponses"
        , optionKey = True
        , optionPoints = 1
        }

    q014 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 14
               , stemText = Textarea "Les tableaux en Java sont:"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d014_1 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "a)"
        , optionText = Textarea "Références d'objet"
        , optionKey = False
        , optionPoints = 0
        }

    k014 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "b)"
        , optionText = Textarea "objets"
        , optionKey = True
        , optionPoints = 1
        }

    d014_2 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "c)"
        , optionText = Textarea "Type de données primitif"
        , optionKey = False
        , optionPoints = 0
        }

    d014_3 <- insert $ Option
        { optionStem = q014
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucune"
        , optionKey = False
        , optionPoints = 0
        }

    q015 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 15
               , stemText = Textarea "Identifier la définition corrigée d'un package."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d015_1 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "a)"
        , optionText = Textarea "Un package est un ensemble d'outils d'édition"
        , optionKey = False
        , optionPoints = 0
        }

    d015_2 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "b)"
        , optionText = Textarea "Un package est un ensemble de classes"
        , optionKey = False
        , optionPoints = 0
        }

    k015 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "c)"
        , optionText = Textarea "Un package est un ensemble de classes et d'interfaces"
        , optionKey = True
        , optionPoints = 1
        }

    d015_3 <- insert $ Option
        { optionStem = q015
        , optionOrdinal = "d)"
        , optionText = Textarea "Un package est un ensemble d'interfaces"
        , optionKey = False
        , optionPoints = 0
        }

    q016 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 16
               , stemText = Textarea [st|Identifiez la restriction correcte sur les méthodes statiques.
  <ol>
    <li>Ils ne doivent accéder qu'aux données statiques.</li>
    <li>Ils ne peuvent appeler que d'autres méthodes statiques.</li>
    <li>Ils ne peuvent pas se référer à <code>this</code> ou à <code>super</code>.</li>
  </ol>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d016_1 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "a)"
        , optionText = Textarea "I et II"
        , optionKey = False
        , optionPoints = 0
        }

    d016_2 <- insert $ Option
        { optionStem = q016
        , optionOrdinal = "b)"
        , optionText = Textarea "II et III"
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
        , optionText = Textarea "I, II et III"
        , optionKey = True
        , optionPoints = 1
        }

    q017 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 17
               , stemText = Textarea "Identifiez le mot-clé parmi les suivants qui fait qu'une variable appartient à une classe, plutôt que d'être définie pour chaque instance de la classe."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea [st|Identifier ce qui peut accéder directement et modifier la valeur de la variable <code>res</code>.
<code><pre>
  package com.mypackage;
  public class Solution{
      private int res = 100;
  }
</pre></code>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d018_1 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "a)"
        , optionText = Textarea "N'importe quelle classe"
        , optionKey = False
        , optionPoints = 0
        }

    k018 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "b)"
        , optionText = Textarea "Seule la classe Solution"
        , optionKey = True
        , optionPoints = 1
        }

    d018_2 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "c)"
        , optionText = Textarea "Toute classe qui étend Solution"
        , optionKey = False
        , optionPoints = 0
        }

    d018_3 <- insert $ Option
        { optionStem = q018
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucune"
        , optionKey = False
        , optionPoints = 0
        }

    q019 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 19
               , stemText = Textarea "Dans lequel des éléments suivants la méthode <code>toString()</code> est-elle définie ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Aucune"
        , optionKey = False
        , optionPoints = 0
        }

    q020 <- insert $ Stem
               { stemTest = t001
               , stemSkill = s001
               , stemOrdinal = 20
               , stemText = Textarea "<code>compareTo()</code> renvoie"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Une valeur <code>int</code>"
        , optionKey = True
        , optionPoints = 1
        }

    d020_3 <- insert $ Option
        { optionStem = q020
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucune"
        , optionKey = False
        , optionPoints = 0
        }

    e001 <- insert $ Exam
        { examTest = t001
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , skillName = "Bases de la programmation Python"
        , skillDescr = Just "Compétences pour la programmation en Python"
        }

    t101 <- insert $ Test
        { testCode = "E201"
        , testName = "Introduction à la programmation Python"
        , testDuration = 10
        , testPass = 8
        , testDescr = Just $ Textarea "Testez les compétences de base en programmation Python"
        , testState = TestStatePublished
        }

    q101 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 1
               , stemText = Textarea "Qui a développé le langage de programmation Python ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quel type de programmation Python prend-il en charge ?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Sélectionnez tout vrai"
               }

    k102_1 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "a)"
        , optionText = Textarea "programmation orientée objet"
        , optionKey = True
        , optionPoints = 1
        }

    k102_2 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "b)"
        , optionText = Textarea "programmation structurée"
        , optionKey = True
        , optionPoints = 1
        }

    k102_3 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "c)"
        , optionText = Textarea "programmation fonctionnelle"
        , optionKey = True
        , optionPoints = 1
        }
        
    d102 <- insert $ Option
        { optionStem = q102
        , optionOrdinal = "d)"
        , optionText = Textarea "programmation procédurale"
        , optionKey = False
        , optionPoints = 0
        }

    q103 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 3
               , stemText = Textarea "Python est-il sensible à la casse lorsqu'il s'agit d'identificateurs ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d103_1 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "a)"
        , optionText = Textarea "Non"
        , optionKey = False
        , optionPoints = 0
        }

    k103 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "b)"
        , optionText = Textarea "Oui"
        , optionKey = True
        , optionPoints = 1
        }

    d103_2 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "c)"
        , optionText = Textarea "dépendant de la machine"
        , optionKey = False
        , optionPoints = 0
        }
        
    d103_3 <- insert $ Option
        { optionStem = q103
        , optionOrdinal = "d)"
        , optionText = Textarea "aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q104 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 4
               , stemText = Textarea "Laquelle des extensions suivantes est l'extension correcte du fichier Python ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Le code Python est-il compilé ou interprété ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k105 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "a)"
        , optionText = Textarea "Le code Python est à la fois compilé et interprété"
        , optionKey = True
        , optionPoints = 1
        }

    d105_1 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "b)"
        , optionText = Textarea "Le code Python n'est ni compilé ni interprété"
        , optionKey = False
        , optionPoints = 0
        }

    d105_2 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "c)"
        , optionText = Textarea "Le code Python est seulement compilé"
        , optionKey = False
        , optionPoints = 0
        }
        
    d105_3 <- insert $ Option
        { optionStem = q105
        , optionOrdinal = "d)"
        , optionText = Textarea "Le code Python est uniquement interprété"
        , optionKey = False
        , optionPoints = 0
        }

    q106 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 6
               , stemText = Textarea "Tous les mots-clés en Python sont dans&nbsp;_________"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d106_1 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "a)"
        , optionText = Textarea "En majuscule"
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
        , optionText = Textarea "HAUT DE CASSE"
        , optionKey = False
        , optionPoints = 0
        }
        
    k106 <- insert $ Option
        { optionStem = q106
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = True
        , optionPoints = 1
        }

    q107 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 7
               , stemText = Textarea [st|Quelle sera la valeur de l'expression Python suivante ?
<code>
<pre>
  4 + 3 % 5
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Lequel des éléments suivants est utilisé pour définir un bloc de code en langage Python ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k108 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "a)"
        , optionText = Textarea "Échancrure"
        , optionKey = True
        , optionPoints = 1
        }

    d108_1 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "b)"
        , optionText = Textarea "Clé"
        , optionKey = False
        , optionPoints = 0
        }

    d108_2 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "c)"
        , optionText = Textarea "Supports"
        , optionKey = False
        , optionPoints = 0
        }
        
    d108_3 <- insert $ Option
        { optionStem = q108
        , optionOrdinal = "d)"
        , optionText = Textarea "Tous les mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q109 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 9
               , stemText = Textarea "Quel mot clé est utilisé pour la fonction en langage Python ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Lequel des caractères suivants est utilisé pour donner des commentaires sur une seule ligne en Python ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea [st|Quelle sera la sortie du code Python suivant ?
<pre>
<code>
  i = 1
  while True:
      if i%3 == 0:
          break
      print(i)

      i + = 1
</cpde>
</pre>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "erreur"
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
        , optionText = Textarea "aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q112 <- insert $ Stem
               { stemTest = t101
               , stemSkill = s101
               , stemOrdinal = 12
               , stemText = Textarea "Laquelle des fonctions suivantes peut nous aider à trouver la version de python sur laquelle nous travaillons actuellement ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        { skillCode = "Génie chimique de base"
        , skillName = "Génie chimique de base"
        , skillDescr = Just "Compétences pour le génie chimique de base"
        }

    t201 <- insert $ Test
        { testCode = "E202"
        , testName = "Ingénieur chimiste"
        , testDuration = 20
        , testPass = 8
        , testDescr = Just $ Textarea "Tester les compétences de base en génie chimique"
        , testState = TestStatePublished
        }

    q201 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 1
               , stemText = Textarea "Quelle est l'unité de gravité spécifique ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k201 <- insert $ Option
        { optionStem = q201
        , optionOrdinal = "a)"
        , optionText = Textarea "Adimensionnelle"
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
               , stemText = Textarea "Lequel des éléments suivants a le même nombre de moles que dans 398 grammes de CuSO<sub>4</sub> ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d202_1 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "a)"
        , optionText = Textarea "35 grammes d'azote"
        , optionKey = False
        , optionPoints = 0
        }

    d202_2 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "b)"
        , optionText = Textarea "58,5 grammes de chlorure de sodium"
        , optionKey = False
        , optionPoints = 0
        }

    d202_3 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "c)"
        , optionText = Textarea "2 grammes d'hydrogène"
        , optionKey = False
        , optionPoints = 0
        }
        
    k202 <- insert $ Option
        { optionStem = q202
        , optionOrdinal = "d)"
        , optionText = Textarea "40 grammes d'oxygène"
        , optionKey = True
        , optionPoints = 1
        }

    q203 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 3
               , stemText = Textarea "Quelle est la densité de 5 Kg d'eau occupée dans 10 m<sup>3</sup> par rapport à 500 g/m<sup>3</sup> ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quelle est l'unité de la fraction molaire ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = True
        , optionPoints = 1
        }

    q205 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 5
               , stemText = Textarea "Quel est le poids de 10 moles d'un mélange de composition 15 % O<sub>2</sub>, 25 % SO<sub>2</sub>, 30 % COCl<sub>2</sub>, 25 % SO<sub>3</sub> et 5 % N<sub>2</sub> ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Qu'est-ce que 100<sup>o</sup>C en degrés Fahrenheit ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quelle est la pression de 1900 Torr dans le bar ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Lequel des éléments suivants n'est pas un appareil de mesure de la pression ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k208 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "a)"
        , optionText = Textarea "Galvanomètre"
        , optionKey = True
        , optionPoints = 1
        }

    d208_1 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "b)"
        , optionText = Textarea "Manomètre"
        , optionKey = False
        , optionPoints = 0
        }

    d208_2 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "c)"
        , optionText = Textarea "Baromètre"
        , optionKey = False
        , optionPoints = 0
        }
        
    d208_3 <- insert $ Option
        { optionStem = q208
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q209 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 9
               , stemText = Textarea "Lequel des éléments suivants est utilisé pour mesurer la pression d'un liquide uniquement ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d209_1 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "a)"
        , optionText = Textarea "Manomètre différentiel"
        , optionKey = False
        , optionPoints = 0
        }

    d209_2 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "b)"
        , optionText = Textarea "Manomètre"
        , optionKey = False
        , optionPoints = 0
        }

    k209 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "c)"
        , optionText = Textarea "Piézomètre"
        , optionKey = True
        , optionPoints = 1
        }
        
    d209_3 <- insert $ Option
        { optionStem = q209
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q210 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 10
               , stemText = Textarea "Laquelle des fonctions suivantes est une fonction d'état ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k210 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "a)"
        , optionText = Textarea "Entropie"
        , optionKey = True
        , optionPoints = 1
        }

    d210_1 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "b)"
        , optionText = Textarea "Chaleur"
        , optionKey = False
        , optionPoints = 0
        }

    d210_2 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "c)"
        , optionText = Textarea "Travail"
        , optionKey = False
        , optionPoints = 0
        }
        
    d210_3 <- insert $ Option
        { optionStem = q210
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    q211 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 11
               , stemText = Textarea "L'eau bouillante dans un récipient est un exemple de laquelle des propositions suivantes ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Batch et Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }
        
    k211 <- insert $ Option
        { optionStem = q211
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun d'eux"
        , optionKey = True
        , optionPoints = 1
        }

    q212 <- insert $ Stem
               { stemTest = t201
               , stemSkill = s201
               , stemOrdinal = 12
               , stemText = Textarea "Laquelle des affirmations suivantes est vraie concernant la limitation des réactifs ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d212_1 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "a)"
        , optionText = Textarea "Consomme partiellement"
        , optionKey = False
        , optionPoints = 0
        }

    d212_2 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "b)"
        , optionText = Textarea "Ne réagit pas"
        , optionKey = False
        , optionPoints = 0
        }

    k212 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "c)"
        , optionText = Textarea "Consomme complètement"
        , optionKey = True
        , optionPoints = 1
        }
        
    d212_3 <- insert $ Option
        { optionStem = q212
        , optionOrdinal = "d)"
        , optionText = Textarea "Aucun des mentionnés"
        , optionKey = False
        , optionPoints = 0
        }

    e201 <- insert $ Exam
        { examTest = t201
        , examCandidate = c001
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
                , skillDescr = Just "Skills for writing SQL"
                }

    t301 <- insert $ Test
        { testCode = "E301"
        , testName = "SQL"
        , testDuration = 30
        , testPass = 9
        , testDescr = Just $ Textarea "Programmation SQL"
        , testState = TestStatePublished
        }

    q301 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 1
               , stemText = Textarea "Quelle est la forme complète de SQL ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , optionText = Textarea "Aucun d'eux"
        , optionKey = False
        , optionPoints = 0
        }

    q302 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 2
               , stemText = Textarea "Lequel des éléments suivants n'est pas un type SQL valide ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Laquelle des commandes suivantes n'est pas une commande DDL ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Parmi les commandes suivantes, lesquelles sont des commandes TCL ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k304 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "a."
        , optionText = Textarea "COMMIT et ROLLBACK"
        , optionKey = True
        , optionPoints = 1
        }

    d304_1 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "b."
        , optionText = Textarea "UPDATE et TRUNCATE"
        , optionKey = False
        , optionPoints = 0
        }

    d304_2 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "c."
        , optionText = Textarea "SELECT et INSERT"
        , optionKey = False
        , optionPoints = 0
        }

    d304_3 <- insert $ Option
        { optionStem = q304
        , optionOrdinal = "d."
        , optionText = Textarea "GRANT et REVOKE"
        , optionKey = False
        , optionPoints = 0
        }

    q305 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 5
               , stemText = Textarea "Quelle instruction est utilisée pour supprimer toutes les lignes d'une table sans que l'action soit consignée ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Les vues SQL sont également appelées"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    d306_1 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "a."
        , optionText = Textarea "Tableaux simples"
        , optionKey = False
        , optionPoints = 0
        }

    k306 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "b."
        , optionText = Textarea "Tables virtuelles"
        , optionKey = True
        , optionPoints = 1
        }

    d306_2 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "c."
        , optionText = Textarea "Tableaux complexes"
        , optionKey = False
        , optionPoints = 0
        }

    d306_3 <- insert $ Option
        { optionStem = q306
        , optionOrdinal = "d."
        , optionText = Textarea "Tableaux réels"
        , optionKey = False
        , optionPoints = 0
        }

    q307 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 7
               , stemText = Textarea "Combien de clés primaires peut-on avoir dans une table ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
               }

    k307 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "a."
        , optionText = Textarea "Seulement 1"
        , optionKey = True
        , optionPoints = 1
        }

    d307_1 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "b."
        , optionText = Textarea "Seulement 2"
        , optionKey = False
        , optionPoints = 0
        }

    d307_2 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "c."
        , optionText = Textarea "Dépend du nombre de colonnes"
        , optionKey = False
        , optionPoints = 0
        }

    d307_3 <- insert $ Option
        { optionStem = q307
        , optionOrdinal = "d."
        , optionText = Textarea "Dépend du DBA"
        , optionKey = False
        , optionPoints = 0
        }

    q308 <- insert $ Stem
               { stemTest = t301
               , stemSkill = s301
               , stemOrdinal = 8
               , stemText = Textarea "Quel type de données peut stocker des données non structurées dans une colonne ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Lequel des éléments suivants n'est pas une contrainte en SQL ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Laquelle des fonctions suivantes n'est pas une fonction d'agrégation valide ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quelle commande de manipulation de données est utilisée pour combiner les enregistrements d'une ou plusieurs tables ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
               , stemText = Textarea "Quel opérateur est utilisé pour comparer une valeur à une liste de valeurs spécifiée ?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Choisissez une"
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
        , examStatus = ExamStatusCompleted
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
