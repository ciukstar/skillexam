{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoDataFR (populateFR) where

import qualified Data.ByteString.Base64 as B64 (decode)
import Text.Shakespeare.Text (st)
import ClassyPrelude.Yesod (ReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay), addUTCTime)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert, insert_) )
import ClassyPrelude.Yesod (ReaderT, forM_, Textarea (Textarea))

import Model
    ( Skill(Skill, skillCode, skillName, skillDescr)
    , Candidate (Candidate, candidateFamilyName, candidateGivenName, candidateAdditionalName, candidateBday)
    , Photo (Photo, photoCandidate, photoPhoto, photoMime)
    , Test (Test, testCode, testName, testDuration, testPass, testDescr, testState)
    , TestState (TestStatePublished)
    , Stem (Stem, stemTest, stemSkill, stemOrdinal, stemText, stemType, stemInstruc)
    , StemType (SingleRespose, MultiResponse)
    , Option (Option, optionStem, optionOrdinal, optionText, optionKey, optionPoints)
    , Answer (Answer, answerExam, answerStem, answerOption, answerTime)
    , Exam (Exam, examTest, examCandidate, examAttempt, examStart, examEnd)
    )

import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )

populateFR :: MonadIO m => ReaderT SqlBackend m ()
populateFR = do
    (now,today) <- liftIO $ getCurrentTime >>= \x -> return (x,utctDay x)

    c001 <- insert $ Candidate
               { candidateFamilyName = "Martin"
               , candidateGivenName = "Léo"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-28) today
               }

    case B64.decode man01 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c001
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c002 <- insert $ Candidate
               { candidateFamilyName = "Bernard"
               , candidateGivenName = "Jade"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-26) today
               }

    case B64.decode woman01 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c002
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c003 <- insert $ Candidate
               { candidateFamilyName = "Thomas"
               , candidateGivenName = "Gabriel"
               , candidateAdditionalName = Just "Raphaël"
               , candidateBday = Just $ addGregorianYearsClip (-21) today
               }

    case B64.decode man02 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c003
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c004 <- insert $ Candidate
               { candidateFamilyName = "Robert"
               , candidateGivenName = "Louise"
               , candidateAdditionalName = Just "Emma"
               , candidateBday = Just $ addGregorianYearsClip (-30) today
               }

    case B64.decode woman02 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c004
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c005 <- insert $ Candidate
               { candidateFamilyName = "Richard"
               , candidateGivenName = "Arthur"
               , candidateAdditionalName = Just "Louis"
               , candidateBday = Just $ addGregorianYearsClip (-32) today
               }

    case B64.decode man03 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c005
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c006 <- insert $ Candidate
               { candidateFamilyName = "Durand"
               , candidateGivenName = "Jules"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               }

    case B64.decode man04 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c006
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c007 <- insert $ Candidate
               { candidateFamilyName = "Dubois"
               , candidateGivenName = "Alice"
               , candidateAdditionalName = Just "Ambre"
               , candidateBday = Just $ addGregorianYearsClip (-35) today
               }

    case B64.decode woman03 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c007
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c008 <- insert $ Candidate
               { candidateFamilyName = "Moreau"
               , candidateGivenName = "Lina"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-42) today
               }

    case B64.decode woman04 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c008
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c009 <- insert $ Candidate
               { candidateFamilyName = "Laurent"
               , candidateGivenName = "Adam"
               , candidateAdditionalName = Nothing
               , candidateBday = Just $ addGregorianYearsClip (-46) today
               }

    case B64.decode man05 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c009
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c010 <- insert $ Candidate
               { candidateFamilyName = "Simon"
               , candidateGivenName = "Maël"
               , candidateAdditionalName = Just "Lucas"
               , candidateBday = Just $ addGregorianYearsClip (-39) today
               }

    case B64.decode man06 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c010
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }

    c011 <- insert $ Candidate
               { candidateFamilyName = "Michel"
               , candidateGivenName = "Rose"
               , candidateAdditionalName = Just "Chloé"
               , candidateBday = Just $ addGregorianYearsClip (-31) today
               }

    case B64.decode woman05 of
      Left _ -> return ()
      Right photo -> insert_ $ Photo { photoCandidate = c011
                                     , photoPhoto = photo
                                     , photoMime = "image/avif"
                                     }
                     
    skill001 <- insert $ Skill
        { skillCode = "Jakarta EE"
        , skillName = "Java Enterprise Edition"
        , skillDescr = Just "Compétences en programmation en Java Enterprise Edition"
        }

    test001 <- insert $ Test
        { testCode = "E101"
        , testName = "Java Programming Basics"
        , testDuration = 120
        , testPass = 25
        , testDescr = Just $ Textarea "Test basic Java Programming Skills"
        , testState = TestStatePublished
        }

    stem001 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 1
               , stemText = Textarea "Who invented Java Programming?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis001_1 <- insert $ Option
        { optionStem = stem001
        , optionOrdinal = "a)"
        , optionText = Textarea "Guido van Rossum"
        , optionKey = False
        , optionPoints = 0
        }

    key001 <- insert $ Option
        { optionStem = stem001
        , optionOrdinal = "b)"
        , optionText = Textarea "James Gosling"
        , optionKey = True
        , optionPoints = 3
        }

    dis001_2 <- insert $ Option
        { optionStem = stem001
        , optionOrdinal = "c)"
        , optionText = Textarea "Dennis Ritchie"
        , optionKey = False
        , optionPoints = 0
        }

    dis001_3 <- insert $ Option
        { optionStem = stem001
        , optionOrdinal = "d)"
        , optionText = Textarea "Bjarne Stroustrup"
        , optionKey = False
        , optionPoints = 0
        }

    stem002 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 2
               , stemText = Textarea "Which statement is true about Java?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Select all correct"
               }

    dis002_1 <- insert $ Option
        { optionStem = stem002
        , optionOrdinal = "a)"
        , optionText = Textarea "Java is a sequence-dependent programming language"
        , optionKey = False
        , optionPoints = 0
        }

    dis002_2 <- insert $ Option
        { optionStem = stem002
        , optionOrdinal = "b)"
        , optionText = Textarea "Java is a code dependent programming language"
        , optionKey = False
        , optionPoints = 0
        }

    key002_1 <- insert $ Option
        { optionStem = stem002
        , optionOrdinal = "c)"
        , optionText = Textarea "Java is a platform-dependent programming language"
        , optionKey = True
        , optionPoints = 2
        }

    key002_2 <- insert $ Option
        { optionStem = stem002
        , optionOrdinal = "d)"
        , optionText = Textarea "Java is a platform-independent programming language"
        , optionKey = True
        , optionPoints = 5
        }

    stem003 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 3
               , stemText = Textarea "Which component is used to compile, debug and execute the java programs?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis003_1 <- insert $ Option
        { optionStem = stem003
        , optionOrdinal = "a)"
        , optionText = Textarea "JRE"
        , optionKey = False
        , optionPoints = 0
        }

    dis003_2 <- insert $ Option
        { optionStem = stem003
        , optionOrdinal = "b)"
        , optionText = Textarea "JIT"
        , optionKey = False
        , optionPoints = 0
        }

    key003 <- insert $ Option
        { optionStem = stem003
        , optionOrdinal = "c)"
        , optionText = Textarea "JDK"
        , optionKey = True
        , optionPoints = 3
        }

    dis003_3 <- insert $ Option
        { optionStem = stem003
        , optionOrdinal = "d)"
        , optionText = Textarea "JVM"
        , optionKey = False
        , optionPoints = 0
        }

    stem004 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 4
               , stemText = Textarea "Number of primitive data types in Java are?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis004_1 <- insert $ Option
        { optionStem = stem004
        , optionOrdinal = "a)"
        , optionText = Textarea "6"
        , optionKey = False
        , optionPoints = 0
        }

    dis004_2 <- insert $ Option
        { optionStem = stem004
        , optionOrdinal = "b)"
        , optionText = Textarea "7"
        , optionKey = False
        , optionPoints = 0
        }

    key004 <- insert $ Option
        { optionStem = stem004
        , optionOrdinal = "c)"
        , optionText = Textarea "8"
        , optionKey = True
        , optionPoints = 1
        }

    dis004_3 <- insert $ Option
        { optionStem = stem004
        , optionOrdinal = "d)"
        , optionText = Textarea "9"
        , optionKey = False
        , optionPoints = 0
        }

    stem005 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 5
               , stemText = Textarea "What is the size of float and double in java?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key005 <- insert $ Option
        { optionStem = stem005
        , optionOrdinal = "a)"
        , optionText = Textarea "32 and 64"
        , optionKey = True
        , optionPoints = 1
        }

    dis005_1 <- insert $ Option
        { optionStem = stem005
        , optionOrdinal = "b)"
        , optionText = Textarea "32 and 32"
        , optionKey = False
        , optionPoints = 0
        }

    dis005_2 <- insert $ Option
        { optionStem = stem005
        , optionOrdinal = "c)"
        , optionText = Textarea "64 and 64"
        , optionKey = False
        , optionPoints = 0
        }

    dis005_3 <- insert $ Option
        { optionStem = stem005
        , optionOrdinal = "d)"
        , optionText = Textarea "64 and 32"
        , optionKey = False
        , optionPoints = 0
        }

    stem006 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 6
               , stemText = Textarea "Automatic type conversion is possible in which of the possible cases?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis006_1 <- insert $ Option
        { optionStem = stem006
        , optionOrdinal = "a)"
        , optionText = Textarea "Byte to int"
        , optionKey = False
        , optionPoints = 0
        }

    key006 <- insert $ Option
        { optionStem = stem006
        , optionOrdinal = "b)"
        , optionText = Textarea "Int to long"
        , optionKey = True
        , optionPoints = 1
        }

    dis006_2 <- insert $ Option
        { optionStem = stem006
        , optionOrdinal = "c)"
        , optionText = Textarea "Long to int"
        , optionKey = False
        , optionPoints = 0
        }

    dis006_3 <- insert $ Option
        { optionStem = stem006
        , optionOrdinal = "d)"
        , optionText = Textarea "Short to int"
        , optionKey = False
        , optionPoints = 0
        }

    stem007 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 7
               , stemText = Textarea [st|Find the output of the following code.
<code>
<pre>
  int Integer = 24;
  char String = ‘I’;
  System.out.print(Integer);
  System.out.print(String);
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis007_1 <- insert $ Option
        { optionStem = stem007
        , optionOrdinal = "a)"
        , optionText = Textarea "Compile error"
        , optionKey = False
        , optionPoints = 0
        }

    dis007_2 <- insert $ Option
        { optionStem = stem007
        , optionOrdinal = "b)"
        , optionText = Textarea "Throws exception"
        , optionKey = False
        , optionPoints = 0
        }

    dis007_3 <- insert $ Option
        { optionStem = stem007
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>I</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key007 <- insert $ Option
        { optionStem = stem007
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>24 I</code>"
        , optionKey = True
        , optionPoints = 1
        }

    stem008 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 8
               , stemText = Textarea [st|Find the output of the following program.
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
               , stemInstruc = Textarea "Select one"
               }

    dis008_1 <- insert $ Option
        { optionStem = stem008
        , optionOrdinal = "a)"
        , optionText = Textarea "50"
        , optionKey = False
        , optionPoints = 0
        }

    dis008_2 <- insert $ Option
        { optionStem = stem008
        , optionOrdinal = "b)"
        , optionText = Textarea "10"
        , optionKey = False
        , optionPoints = 0
        }

    key008 <- insert $ Option
        { optionStem = stem008
        , optionOrdinal = "c)"
        , optionText = Textarea "Compile error"
        , optionKey = True
        , optionPoints = 1
        }

    dis008_3 <- insert $ Option
        { optionStem = stem008
        , optionOrdinal = "d)"
        , optionText = Textarea "Exception"
        , optionKey = False
        , optionPoints = 0
        }

    stem009 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 9
               , stemText = Textarea "Select the valid statement."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis009_1 <- insert $ Option
        { optionStem = stem009
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>char[] ch = new char(5)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key009 <- insert $ Option
        { optionStem = stem009
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>char[] ch = new char[5]</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis009_2 <- insert $ Option
        { optionStem = stem009
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>char[] ch = new char()</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis009_3 <- insert $ Option
        { optionStem = stem009
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>char[] ch = new char[]</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem010 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 10
               , stemText = Textarea [st|Find the output of the following program.
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
               , stemInstruc = Textarea "Select one"
               }

    dis010_1 <- insert $ Option
        { optionStem = stem010
        , optionOrdinal = "a)"
        , optionText = Textarea "120 200 016"
        , optionKey = False
        , optionPoints = 0
        }

    key010 <- insert $ Option
        { optionStem = stem010
        , optionOrdinal = "b)"
        , optionText = Textarea "120 200 14"
        , optionKey = True
        , optionPoints = 1
        }

    dis010_2 <- insert $ Option
        { optionStem = stem010
        , optionOrdinal = "c)"
        , optionText = Textarea "120 200 16"
        , optionKey = False
        , optionPoints = 0
        }

    dis010_3 <- insert $ Option
        { optionStem = stem010
        , optionOrdinal = "d)"
        , optionText = Textarea "None"
        , optionKey = False
        , optionPoints = 0
        }

    stem011 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 11
               , stemText = Textarea "When an array is passed to a method, what does the method receive?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key011 <- insert $ Option
        { optionStem = stem011
        , optionOrdinal = "a)"
        , optionText = Textarea "The reference of the array"
        , optionKey = True
        , optionPoints = 1
        }

    dis011_1 <- insert $ Option
        { optionStem = stem011
        , optionOrdinal = "b)"
        , optionText = Textarea "A copy of the array"
        , optionKey = False
        , optionPoints = 0
        }

    dis011_2 <- insert $ Option
        { optionStem = stem011
        , optionOrdinal = "c)"
        , optionText = Textarea "Length of the array"
        , optionKey = False
        , optionPoints = 0
        }

    dis011_3 <- insert $ Option
        { optionStem = stem011
        , optionOrdinal = "d)"
        , optionText = Textarea "Copy of first element"
        , optionKey = False
        , optionPoints = 0
        }

    stem012 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 12
               , stemText = Textarea "Select the valid statement to declare and initialize an array."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis012_1 <- insert $ Option
        { optionStem = stem012
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>int[] A = {}</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key012 <- insert $ Option
        { optionStem = stem012
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>int[] A = {1, 2, 3}</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis012_2 <- insert $ Option
        { optionStem = stem012
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>int[] A = (1, 2, 3)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis012_3 <- insert $ Option
        { optionStem = stem012
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>int[][] A = {1,2,3}</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem013 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 13
               , stemText = Textarea "Find the value of <code>A[1]</code> after execution of the following program."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis013_1 <- insert $ Option
        { optionStem = stem013
        , optionOrdinal = "a)"
        , optionText = Textarea "0"
        , optionKey = False
        , optionPoints = 0
        }

    key013 <- insert $ Option
        { optionStem = stem013
        , optionOrdinal = "b)"
        , optionText = Textarea "1"
        , optionKey = True
        , optionPoints = 1
        }

    dis013_2 <- insert $ Option
        { optionStem = stem013
        , optionOrdinal = "c)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    dis013_3 <- insert $ Option
        { optionStem = stem013
        , optionOrdinal = "d)"
        , optionText = Textarea "3"
        , optionKey = False
        , optionPoints = 0
        }

    stem014 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 14
               , stemText = Textarea "Arrays in java are-"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis014_1 <- insert $ Option
        { optionStem = stem014
        , optionOrdinal = "a)"
        , optionText = Textarea "Object references"
        , optionKey = False
        , optionPoints = 0
        }

    key014 <- insert $ Option
        { optionStem = stem014
        , optionOrdinal = "b)"
        , optionText = Textarea "objects"
        , optionKey = True
        , optionPoints = 1
        }

    dis014_2 <- insert $ Option
        { optionStem = stem014
        , optionOrdinal = "c)"
        , optionText = Textarea "Primitive data type"
        , optionKey = False
        , optionPoints = 0
        }

    dis014_3 <- insert $ Option
        { optionStem = stem014
        , optionOrdinal = "d)"
        , optionText = Textarea "None"
        , optionKey = False
        , optionPoints = 0
        }

    stem015 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 15
               , stemText = Textarea "Identify the corrected definition of a package."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis015_1 <- insert $ Option
        { optionStem = stem015
        , optionOrdinal = "a)"
        , optionText = Textarea "A package is a collection of editing tools"
        , optionKey = False
        , optionPoints = 0
        }

    dis015_2 <- insert $ Option
        { optionStem = stem015
        , optionOrdinal = "b)"
        , optionText = Textarea "A package is a collection of classes"
        , optionKey = False
        , optionPoints = 0
        }

    key015 <- insert $ Option
        { optionStem = stem015
        , optionOrdinal = "c)"
        , optionText = Textarea "A package is a collection of classes and interfaces"
        , optionKey = True
        , optionPoints = 1
        }

    dis015_3 <- insert $ Option
        { optionStem = stem015
        , optionOrdinal = "d)"
        , optionText = Textarea "A package is a collection of interfaces"
        , optionKey = False
        , optionPoints = 0
        }

    stem016 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 16
               , stemText = Textarea [st|Identify the correct restriction on static methods.
  <ol>
    <li>They must access only static data</li>
    <li>They can only call other static methods.</li>
    <li>They cannot refer to this or super.</li>
  </ol>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis016_1 <- insert $ Option
        { optionStem = stem016
        , optionOrdinal = "a)"
        , optionText = Textarea "I and II"
        , optionKey = False
        , optionPoints = 0
        }

    dis016_2 <- insert $ Option
        { optionStem = stem016
        , optionOrdinal = "b)"
        , optionText = Textarea "II and III"
        , optionKey = False
        , optionPoints = 0
        }

    dis016_3 <- insert $ Option
        { optionStem = stem016
        , optionOrdinal = "c)"
        , optionText = Textarea "Only III"
        , optionKey = False
        , optionPoints = 0
        }

    key016 <- insert $ Option
        { optionStem = stem016
        , optionOrdinal = "d)"
        , optionText = Textarea "I, II and III"
        , optionKey = True
        , optionPoints = 1
        }

    stem017 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 17
               , stemText = Textarea "Identify the keyword among the following that makes a variable belong to a class,rather than being defined for each instance of the class."
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis017_1 <- insert $ Option
        { optionStem = stem017
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>final</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key017 <- insert $ Option
        { optionStem = stem017
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>static</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis017_2 <- insert $ Option
        { optionStem = stem017
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>volatile</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis017_3 <- insert $ Option
        { optionStem = stem017
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>abstract</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem018 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 18
               , stemText = Textarea [st|Identify what can directly access and change the value of the variable <code>res</code>.
<code>
<pre>
  package com.mypackage;
  public class Solution{
      private int res = 100;
  }
</pre>
</code>
|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis018_1 <- insert $ Option
        { optionStem = stem018
        , optionOrdinal = "a)"
        , optionText = Textarea "Any class"
        , optionKey = False
        , optionPoints = 0
        }

    key018 <- insert $ Option
        { optionStem = stem018
        , optionOrdinal = "b)"
        , optionText = Textarea "Only Solution class"
        , optionKey = True
        , optionPoints = 1
        }

    dis018_2 <- insert $ Option
        { optionStem = stem018
        , optionOrdinal = "c)"
        , optionText = Textarea "Any class that extends Solution"
        , optionKey = False
        , optionPoints = 0
        }

    dis018_3 <- insert $ Option
        { optionStem = stem018
        , optionOrdinal = "d)"
        , optionText = Textarea "None"
        , optionKey = False
        , optionPoints = 0
        }

    stem019 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 19
               , stemText = Textarea "In which of the following is <code>toString()</code> method defined?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key019 <- insert $ Option
        { optionStem = stem019
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>java.lang.Object</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis019_1 <- insert $ Option
        { optionStem = stem019
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>java.lang.String</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis019_2 <- insert $ Option
        { optionStem = stem019
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>java.lang.util</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis019_3 <- insert $ Option
        { optionStem = stem019
        , optionOrdinal = "d)"
        , optionText = Textarea "None"
        , optionKey = False
        , optionPoints = 0
        }

    stem020 <- insert $ Stem
               { stemTest = test001
               , stemSkill = skill001
               , stemOrdinal = 20
               , stemText = Textarea "<code>compareTo()</code> returns"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis020_1 <- insert $ Option
        { optionStem = stem020
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>true</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis020_2 <- insert $ Option
        { optionStem = stem020
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>false</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key020 <- insert $ Option
        { optionStem = stem020
        , optionOrdinal = "c)"
        , optionText = Textarea "An <code>int</code> value"
        , optionKey = True
        , optionPoints = 1
        }

    dis020_3 <- insert $ Option
        { optionStem = stem020
        , optionOrdinal = "d)"
        , optionText = Textarea "None"
        , optionKey = False
        , optionPoints = 0
        }

    r001 <- insert $ Exam
        { examTest = test001
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-20) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (stem001,dis001_3)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,dis005_2)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,dis010_3)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_1)
          , (stem016,key016)
          , (stem017,key017)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r001
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r002 <- insert $ Exam
        { examTest = test001
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-40) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,dis004_1)
          , (stem005,dis005_2)
          , (stem006,dis006_3)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,dis011_1)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,dis014_1)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,dis018_3)
          , (stem019,dis019_3)
          , (stem020,dis020_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r002
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r003 <- insert $ Exam
        { examTest = test001
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-45) now
        , examEnd = pure $ addUTCTime (-10) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,dis004_1)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,dis011_1)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r003
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r004 <- insert $ Exam
        { examTest = test001
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-55) now
        , examEnd = pure $ addUTCTime (-15) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,dis004_1)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r004
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r005 <- insert $ Exam
        { examTest = test001
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-50) now
        , examEnd = pure $ addUTCTime (-15) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r005
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r006 <- insert $ Exam
        { examTest = test001
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-3000) now
        , examEnd = pure $ addUTCTime (-2955) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,dis019_1)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r006
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r007 <- insert $ Exam
        { examTest = test001
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-4000) now
        , examEnd = pure $ addUTCTime (-3950) now
        }

    forM_ [ (stem001,key001)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,dis010_2)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r007
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r008 <- insert $ Exam
        { examTest = test001
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-5000) now
        , examEnd = pure $ addUTCTime (-4965) now
        }

    forM_ [ (stem001,key001)
          , (stem002,dis002_2)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,dis010_2)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,key013)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r008
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r009 <- insert $ Exam
        { examTest = test001
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-6000) now
        , examEnd = pure $ addUTCTime (-5960) now
        }

    forM_ [ (stem001,key001)
          , (stem002,dis002_2)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,key010)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,dis013_3)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r009
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r010 <- insert $ Exam
        { examTest = test001
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-6040) now
        , examEnd = pure $ addUTCTime (-6000) now
        }

    forM_ [ (stem001,dis001_3)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,dis010_2)
          , (stem011,key011)
          , (stem012,key012)
          , (stem013,dis013_3)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r010
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r011 <- insert $ Exam
        { examTest = test001
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-5040) now
        , examEnd = pure $ addUTCTime (-5005) now
        }

    forM_ [ (stem001,dis001_3)
          , (stem002,key002_1)
          , (stem002,key002_2)
          , (stem003,key003)
          , (stem004,key004)
          , (stem005,key005)
          , (stem006,key006)
          , (stem007,key007)
          , (stem008,key008)
          , (stem009,key009)
          , (stem010,dis010_2)
          , (stem011,dis011_2)
          , (stem012,dis012_1)
          , (stem013,dis013_3)
          , (stem014,key014)
          , (stem015,dis015_3)
          , (stem016,key016)
          , (stem017,dis017_1)
          , (stem018,key018)
          , (stem019,key019)
          , (stem020,key020)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r011
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    skill101 <- insert $ Skill
        { skillCode = "Python 101"
        , skillName = "Python programming. Basics"
        , skillDescr = Just "Skills for programming in Python"
        }

    test101 <- insert $ Test
        { testCode = "E201"
        , testName = "Introduction to Python programming"
        , testDuration = 10
        , testPass = 8
        , testDescr = Just $ Textarea "Test basic Python Programming Skills"
        , testState = TestStatePublished
        }

    stem101 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 1
               , stemText = Textarea "Who developed Python Programming Language?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis101_1 <- insert $ Option
        { optionStem = stem101
        , optionOrdinal = "a)"
        , optionText = Textarea "Wick van Rossum"
        , optionKey = False
        , optionPoints = 0
        }

    dis101_2 <- insert $ Option
        { optionStem = stem101
        , optionOrdinal = "b)"
        , optionText = Textarea "Rasmus Lerdorf"
        , optionKey = False
        , optionPoints = 0
        }

    key101 <- insert $ Option
        { optionStem = stem101
        , optionOrdinal = "c)"
        , optionText = Textarea "Guido van Rossum"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis101_3 <- insert $ Option
        { optionStem = stem101
        , optionOrdinal = "d)"
        , optionText = Textarea "Niene Stom"
        , optionKey = False
        , optionPoints = 0
        }

    stem102 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 2
               , stemText = Textarea "Which type of Programming does Python support?"
               , stemType = MultiResponse
               , stemInstruc = Textarea "Select all true"
               }

    key102_1 <- insert $ Option
        { optionStem = stem102
        , optionOrdinal = "a)"
        , optionText = Textarea "object-oriented programming"
        , optionKey = True
        , optionPoints = 1
        }

    key102_2 <- insert $ Option
        { optionStem = stem102
        , optionOrdinal = "b)"
        , optionText = Textarea "structured programming"
        , optionKey = True
        , optionPoints = 1
        }

    key102_3 <- insert $ Option
        { optionStem = stem102
        , optionOrdinal = "c)"
        , optionText = Textarea "functional programming"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis102 <- insert $ Option
        { optionStem = stem102
        , optionOrdinal = "d)"
        , optionText = Textarea "procedural programming"
        , optionKey = False
        , optionPoints = 0
        }

    stem103 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 3
               , stemText = Textarea "Is Python case sensitive when dealing with identifiers?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis103_1 <- insert $ Option
        { optionStem = stem103
        , optionOrdinal = "a)"
        , optionText = Textarea "no"
        , optionKey = False
        , optionPoints = 0
        }

    key103 <- insert $ Option
        { optionStem = stem103
        , optionOrdinal = "b)"
        , optionText = Textarea "yes"
        , optionKey = True
        , optionPoints = 1
        }

    dis103_2 <- insert $ Option
        { optionStem = stem103
        , optionOrdinal = "c)"
        , optionText = Textarea "machine dependent"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis103_3 <- insert $ Option
        { optionStem = stem103
        , optionOrdinal = "d)"
        , optionText = Textarea "none of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem104 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 4
               , stemText = Textarea "Which of the following is the correct extension of the Python file?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis104_1 <- insert $ Option
        { optionStem = stem104
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>.python</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis104_2 <- insert $ Option
        { optionStem = stem104
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>.pl</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key104 <- insert $ Option
        { optionStem = stem104
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>.py</code>"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis104_3 <- insert $ Option
        { optionStem = stem104
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>.p</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem105 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 5
               , stemText = Textarea "Is Python code compiled or interpreted?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key105 <- insert $ Option
        { optionStem = stem105
        , optionOrdinal = "a)"
        , optionText = Textarea "Python code is both compiled and interpreted"
        , optionKey = True
        , optionPoints = 1
        }

    dis105_1 <- insert $ Option
        { optionStem = stem105
        , optionOrdinal = "b)"
        , optionText = Textarea "Python code is neither compiled nor interpreted"
        , optionKey = False
        , optionPoints = 0
        }

    dis105_2 <- insert $ Option
        { optionStem = stem105
        , optionOrdinal = "c)"
        , optionText = Textarea "Python code is only compiled"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis105_3 <- insert $ Option
        { optionStem = stem105
        , optionOrdinal = "d)"
        , optionText = Textarea "Python code is only interpreted"
        , optionKey = False
        , optionPoints = 0
        }

    stem106 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 6
               , stemText = Textarea "All keywords in Python are in&nbsp;_________"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis106_1 <- insert $ Option
        { optionStem = stem106
        , optionOrdinal = "a)"
        , optionText = Textarea "Capitalized"
        , optionKey = False
        , optionPoints = 0
        }

    dis106_2 <- insert $ Option
        { optionStem = stem106
        , optionOrdinal = "b)"
        , optionText = Textarea "lower case"
        , optionKey = False
        , optionPoints = 0
        }

    dis106_3 <- insert $ Option
        { optionStem = stem106
        , optionOrdinal = "c)"
        , optionText = Textarea "UPPER CASE"
        , optionKey = False
        , optionPoints = 0
        }
        
    key106 <- insert $ Option
        { optionStem = stem106
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = True
        , optionPoints = 1
        }

    stem107 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 7
               , stemText = Textarea [st|What will be the value of the following Python expression?
<code>
<pre>
  4 + 3 % 5
</pre>
</code>|]
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key107 <- insert $ Option
        { optionStem = stem107
        , optionOrdinal = "a)"
        , optionText = Textarea "7"
        , optionKey = True
        , optionPoints = 1
        }

    dis107_1 <- insert $ Option
        { optionStem = stem107
        , optionOrdinal = "b)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    dis107_2 <- insert $ Option
        { optionStem = stem107
        , optionOrdinal = "c)"
        , optionText = Textarea "4"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis107_3 <- insert $ Option
        { optionStem = stem107
        , optionOrdinal = "d)"
        , optionText = Textarea "1"
        , optionKey = False
        , optionPoints = 0
        }

    stem108 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 8
               , stemText = Textarea "Which of the following is used to define a block of code in Python language?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key108 <- insert $ Option
        { optionStem = stem108
        , optionOrdinal = "a)"
        , optionText = Textarea "Indentation"
        , optionKey = True
        , optionPoints = 1
        }

    dis108_1 <- insert $ Option
        { optionStem = stem108
        , optionOrdinal = "b)"
        , optionText = Textarea "Key"
        , optionKey = False
        , optionPoints = 0
        }

    dis108_2 <- insert $ Option
        { optionStem = stem108
        , optionOrdinal = "c)"
        , optionText = Textarea "Brackets"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis108_3 <- insert $ Option
        { optionStem = stem108
        , optionOrdinal = "d)"
        , optionText = Textarea "All of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem109 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 9
               , stemText = Textarea "Which keyword is used for function in Python language?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis109_1 <- insert $ Option
        { optionStem = stem109
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>Function</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key109 <- insert $ Option
        { optionStem = stem109
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>def</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis109_2 <- insert $ Option
        { optionStem = stem109
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>Fun</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis109_3 <- insert $ Option
        { optionStem = stem109
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>Define</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem110 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 10
               , stemText = Textarea "Which of the following character is used to give single-line comments in Python?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis110_1 <- insert $ Option
        { optionStem = stem110
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>//</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key110 <- insert $ Option
        { optionStem = stem110
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>#</code>"
        , optionKey = True
        , optionPoints = 1
        }

    dis110_2 <- insert $ Option
        { optionStem = stem110
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>!</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis110_3 <- insert $ Option
        { optionStem = stem110
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>/*</code>"
        , optionKey = False
        , optionPoints = 0
        }

    stem111 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 11
               , stemText = Textarea [st|What will be the output of the following Python code?
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
               , stemInstruc = Textarea "Select one"
               }

    dis111_1 <- insert $ Option
        { optionStem = stem111
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>1 2 3</code>"
        , optionKey = False
        , optionPoints = 0
        }

    key111 <- insert $ Option
        { optionStem = stem111
        , optionOrdinal = "b)"
        , optionText = Textarea "error"
        , optionKey = True
        , optionPoints = 1
        }

    dis111_2 <- insert $ Option
        { optionStem = stem111
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>1 2</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis111_3 <- insert $ Option
        { optionStem = stem111
        , optionOrdinal = "d)"
        , optionText = Textarea "none of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem112 <- insert $ Stem
               { stemTest = test101
               , stemSkill = skill101
               , stemOrdinal = 12
               , stemText = Textarea "Which of the following functions can help us to find the version of python that we are currently working on?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis112_1 <- insert $ Option
        { optionStem = stem112
        , optionOrdinal = "a)"
        , optionText = Textarea "<code>sys.version(1)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis112_2 <- insert $ Option
        { optionStem = stem112
        , optionOrdinal = "b)"
        , optionText = Textarea "<code>sys.version(0)</code>"
        , optionKey = False
        , optionPoints = 0
        }

    dis112_3 <- insert $ Option
        { optionStem = stem112
        , optionOrdinal = "c)"
        , optionText = Textarea "<code>sys.version()</code>"
        , optionKey = False
        , optionPoints = 0
        }
        
    key112 <- insert $ Option
        { optionStem = stem112
        , optionOrdinal = "d)"
        , optionText = Textarea "<code>sys.version</code>"
        , optionKey = True
        , optionPoints = 1
        }

    r101 <- insert $ Exam
        { examTest = test101
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-5040) now
        , examEnd = pure $ addUTCTime (-5005) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,dis103_2)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,dis106_3)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,dis110_2)
          , (stem111,key111)
          , (stem112,dis112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r101
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r102 <- insert $ Exam
        { examTest = test101
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-45) now
        , examEnd = pure $ addUTCTime (-35) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,dis103_2)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,dis106_3)
          , (stem107,key107)
          , (stem108,dis108_2)
          , (stem109,key109)
          , (stem110,dis110_2)
          , (stem111,key111)
          , (stem112,dis112_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r102
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r103 <- insert $ Exam
        { examTest = test101
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-55) now
        , examEnd = pure $ addUTCTime (-47) now
        }

    forM_ [ (stem101,dis101_1)
          , (stem102,key102_1)
          , (stem102,dis102)
          , (stem102,key102_3)
          , (stem103,dis103_2)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,dis106_3)
          , (stem107,key107)
          , (stem108,dis108_2)
          , (stem109,key109)
          , (stem110,dis110_2)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r103
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r104 <- insert $ Exam
        { examTest = test101
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-65) now
        , examEnd = pure $ addUTCTime (-56) now
        }

    forM_ [ (stem101,dis101_1)
          , (stem102,key102_1)
          , (stem102,dis102)
          , (stem102,key102_3)
          , (stem103,dis103_2)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,dis108_2)
          , (stem109,key109)
          , (stem110,dis110_2)
          , (stem111,dis111_1)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r104
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r105 <- insert $ Exam
        { examTest = test101
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-80) now
        , examEnd = pure $ addUTCTime (-50) now
        }

    forM_ [ (stem101,dis101_1)
          , (stem102,key102_1)
          , (stem102,dis102)
          , (stem102,key102_3)
          , (stem103,dis103_2)
          , (stem104,dis104_1)
          , (stem105,dis105_2)
          , (stem106,dis106_3)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,dis110_2)
          , (stem111,dis111_1)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r105
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r106 <- insert $ Exam
        { examTest = test101
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-90) now
        , examEnd = pure $ addUTCTime (-80) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,key103)
          , (stem104,dis104_1)
          , (stem105,key105)
          , (stem106,dis106_3)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,key110)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r106
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r107 <- insert $ Exam
        { examTest = test101
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-100) now
        , examEnd = pure $ addUTCTime (-95) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,key103)
          , (stem104,dis104_1)
          , (stem105,key105)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,key110)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r107
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r108 <- insert $ Exam
        { examTest = test101
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-103) now
        , examEnd = pure $ addUTCTime (-96) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,key103)
          , (stem104,dis104_1)
          , (stem105,dis105_2)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,key110)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r108
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r109 <- insert $ Exam
        { examTest = test101
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-203) now
        , examEnd = pure $ addUTCTime (-194) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem103,key103)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,dis110_1)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r109
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r110 <- insert $ Exam
        { examTest = test101
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-302) now
        , examEnd = pure $ addUTCTime (-296) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_3)
          , (stem103,key103)
          , (stem104,key104)
          , (stem105,key105)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,dis110_1)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r110
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r111 <- insert $ Exam
        { examTest = test101
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-312) now
        , examEnd = pure $ addUTCTime (-302) now
        }

    forM_ [ (stem101,key101)
          , (stem102,key102_1)
          , (stem102,key102_2)
          , (stem102,key102_3)
          , (stem102,dis102)
          , (stem103,key103)
          , (stem104,key104)
          , (stem105,dis105_3)
          , (stem106,key106)
          , (stem107,key107)
          , (stem108,key108)
          , (stem109,key109)
          , (stem110,dis110_1)
          , (stem111,key111)
          , (stem112,key112)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r111
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    skill201 <- insert $ Skill
        { skillCode = "Basic Chemical Engineering"
        , skillName = "Basic Chemical Engineering"
        , skillDescr = Just "Skills for Basic Chemical Engineering"
        }

    test201 <- insert $ Test
        { testCode = "E202"
        , testName = "Chemical Engineering"
        , testDuration = 20
        , testPass = 8
        , testDescr = Just $ Textarea "Test basic Basic Chemical Engineering"
        , testState = TestStatePublished
        }

    stem201 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 1
               , stemText = Textarea "What is the unit of specific gravity?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key201 <- insert $ Option
        { optionStem = stem201
        , optionOrdinal = "a)"
        , optionText = Textarea "Dimensionless"
        , optionKey = True
        , optionPoints = 1
        }

    dis201_1 <- insert $ Option
        { optionStem = stem201
        , optionOrdinal = "b)"
        , optionText = Textarea "m/s<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    dis201_2 <- insert $ Option
        { optionStem = stem201
        , optionOrdinal = "c)"
        , optionText = Textarea "N/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis201_3 <- insert $ Option
        { optionStem = stem201
        , optionOrdinal = "d)"
        , optionText = Textarea "Kg/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    stem202 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 2
               , stemText = Textarea "Which of the following has the same number of moles as in 398 grams of CuSO<sub>4</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis202_1 <- insert $ Option
        { optionStem = stem202
        , optionOrdinal = "a)"
        , optionText = Textarea "35 grams of nitrogen"
        , optionKey = False
        , optionPoints = 0
        }

    dis202_2 <- insert $ Option
        { optionStem = stem202
        , optionOrdinal = "b)"
        , optionText = Textarea "58.5 grams of Sodium chloride"
        , optionKey = False
        , optionPoints = 0
        }

    dis202_3 <- insert $ Option
        { optionStem = stem202
        , optionOrdinal = "c)"
        , optionText = Textarea "2 grams of hydrogen"
        , optionKey = False
        , optionPoints = 0
        }
        
    key202 <- insert $ Option
        { optionStem = stem202
        , optionOrdinal = "d)"
        , optionText = Textarea "40 grams of oxygen"
        , optionKey = True
        , optionPoints = 1
        }

    stem203 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 3
               , stemText = Textarea "What is the specific gravity of 5 Kg of water occupied in 10 m<sup>3</sup> with respect to 500 g/m<sup>3</sup>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis203_1 <- insert $ Option
        { optionStem = stem203
        , optionOrdinal = "a)"
        , optionText = Textarea "2"
        , optionKey = False
        , optionPoints = 0
        }

    dis203_2 <- insert $ Option
        { optionStem = stem203
        , optionOrdinal = "b)"
        , optionText = Textarea "5"
        , optionKey = False
        , optionPoints = 0
        }

    dis203_3 <- insert $ Option
        { optionStem = stem203
        , optionOrdinal = "c)"
        , optionText = Textarea "0.5"
        , optionKey = False
        , optionPoints = 0
        }
        
    key203 <- insert $ Option
        { optionStem = stem203
        , optionOrdinal = "d)"
        , optionText = Textarea "1"
        , optionKey = True
        , optionPoints = 1
        }

    stem204 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 4
               , stemText = Textarea "What is the unit of mole fraction?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis204_1 <- insert $ Option
        { optionStem = stem204
        , optionOrdinal = "a)"
        , optionText = Textarea "N/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    dis204_2 <- insert $ Option
        { optionStem = stem204
        , optionOrdinal = "b)"
        , optionText = Textarea "m<sup>-2</sup>"
        , optionKey = False
        , optionPoints = 0
        }

    dis204_3 <- insert $ Option
        { optionStem = stem204
        , optionOrdinal = "c)"
        , optionText = Textarea "Kg/m<sup>3</sup>"
        , optionKey = False
        , optionPoints = 0
        }
        
    key204 <- insert $ Option
        { optionStem = stem204
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = True
        , optionPoints = 1
        }

    stem205 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 5
               , stemText = Textarea "What is the weight of 10 moles of a mixture with composition 15% O<sub>2</sub>, 25% SO<sub>2</sub>, 30% COCl<sub>2</sub>, 25% SO<sub>3</sub> and 5% N<sub>2</sub>?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis205_1 <- insert $ Option
        { optionStem = stem205
        , optionOrdinal = "a)"
        , optionText = Textarea "564"
        , optionKey = False
        , optionPoints = 0
        }

    dis205_2 <- insert $ Option
        { optionStem = stem205
        , optionOrdinal = "b)"
        , optionText = Textarea "475"
        , optionKey = False
        , optionPoints = 0
        }

    dis205_3 <- insert $ Option
        { optionStem = stem205
        , optionOrdinal = "c)"
        , optionText = Textarea "867"
        , optionKey = False
        , optionPoints = 0
        }
        
    key205 <- insert $ Option
        { optionStem = stem205
        , optionOrdinal = "d)"
        , optionText = Textarea "719"
        , optionKey = True
        , optionPoints = 1
        }

    stem206 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 6
               , stemText = Textarea "What is the 100<sup>o</sup>C in degree Fahrenheit?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis206_1 <- insert $ Option
        { optionStem = stem206
        , optionOrdinal = "a)"
        , optionText = Textarea "100<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    key206 <- insert $ Option
        { optionStem = stem206
        , optionOrdinal = "b)"
        , optionText = Textarea "212<sup>o</sup>F"
        , optionKey = True
        , optionPoints = 1
        }

    dis206_2 <- insert $ Option
        { optionStem = stem206
        , optionOrdinal = "c)"
        , optionText = Textarea "460<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis206_3 <- insert $ Option
        { optionStem = stem206
        , optionOrdinal = "d)"
        , optionText = Textarea "0<sup>o</sup>F"
        , optionKey = False
        , optionPoints = 0
        }

    stem207 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 7
               , stemText = Textarea "What is the pressure of 1900 Torr in the bar?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis207_1 <- insert $ Option
        { optionStem = stem207
        , optionOrdinal = "a)"
        , optionText = Textarea "2.46"
        , optionKey = False
        , optionPoints = 0
        }

    dis207_2 <- insert $ Option
        { optionStem = stem207
        , optionOrdinal = "b)"
        , optionText = Textarea "2.87"
        , optionKey = False
        , optionPoints = 0
        }

    key207 <- insert $ Option
        { optionStem = stem207
        , optionOrdinal = "c)"
        , optionText = Textarea "2.40"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis207_3 <- insert $ Option
        { optionStem = stem207
        , optionOrdinal = "d)"
        , optionText = Textarea "2.68"
        , optionKey = False
        , optionPoints = 0
        }

    stem208 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 8
               , stemText = Textarea "Which of the following is not a pressure measuring device?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key208 <- insert $ Option
        { optionStem = stem208
        , optionOrdinal = "a)"
        , optionText = Textarea "Galvanometer"
        , optionKey = True
        , optionPoints = 1
        }

    dis208_1 <- insert $ Option
        { optionStem = stem208
        , optionOrdinal = "b)"
        , optionText = Textarea "Manometer"
        , optionKey = False
        , optionPoints = 0
        }

    dis208_2 <- insert $ Option
        { optionStem = stem208
        , optionOrdinal = "c)"
        , optionText = Textarea "Barometer"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis208_3 <- insert $ Option
        { optionStem = stem208
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem209 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 9
               , stemText = Textarea "Which of the following is used for the pressure measurement of only liquid?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis209_1 <- insert $ Option
        { optionStem = stem209
        , optionOrdinal = "a)"
        , optionText = Textarea "Differential Manometer"
        , optionKey = False
        , optionPoints = 0
        }

    dis209_2 <- insert $ Option
        { optionStem = stem209
        , optionOrdinal = "b)"
        , optionText = Textarea "Manometer"
        , optionKey = False
        , optionPoints = 0
        }

    key209 <- insert $ Option
        { optionStem = stem209
        , optionOrdinal = "c)"
        , optionText = Textarea "Piezometer"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis209_3 <- insert $ Option
        { optionStem = stem209
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem210 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 10
               , stemText = Textarea "Which of the following is a state function?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key210 <- insert $ Option
        { optionStem = stem210
        , optionOrdinal = "a)"
        , optionText = Textarea "Entropy"
        , optionKey = True
        , optionPoints = 1
        }

    dis210_1 <- insert $ Option
        { optionStem = stem210
        , optionOrdinal = "b)"
        , optionText = Textarea "Heat"
        , optionKey = False
        , optionPoints = 0
        }

    dis210_2 <- insert $ Option
        { optionStem = stem210
        , optionOrdinal = "c)"
        , optionText = Textarea "Work"
        , optionKey = False
        , optionPoints = 0
        }
        
    dis210_3 <- insert $ Option
        { optionStem = stem210
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    stem211 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 11
               , stemText = Textarea "Water boiling in a container is an example of which of the following?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis211_1 <- insert $ Option
        { optionStem = stem211
        , optionOrdinal = "a)"
        , optionText = Textarea "Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }

    dis211_2 <- insert $ Option
        { optionStem = stem211
        , optionOrdinal = "b)"
        , optionText = Textarea "Batch"
        , optionKey = False
        , optionPoints = 0
        }

    dis211_3 <- insert $ Option
        { optionStem = stem211
        , optionOrdinal = "c)"
        , optionText = Textarea "Batch & Semi-batch"
        , optionKey = False
        , optionPoints = 0
        }
        
    key211 <- insert $ Option
        { optionStem = stem211
        , optionOrdinal = "d)"
        , optionText = Textarea "Neither of them"
        , optionKey = True
        , optionPoints = 1
        }

    stem212 <- insert $ Stem
               { stemTest = test201
               , stemSkill = skill201
               , stemOrdinal = 12
               , stemText = Textarea "Which of the following is true about limiting reagents?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis212_1 <- insert $ Option
        { optionStem = stem212
        , optionOrdinal = "a)"
        , optionText = Textarea "Consumes partially"
        , optionKey = False
        , optionPoints = 0
        }

    dis212_2 <- insert $ Option
        { optionStem = stem212
        , optionOrdinal = "b)"
        , optionText = Textarea "Does not react"
        , optionKey = False
        , optionPoints = 0
        }

    key212 <- insert $ Option
        { optionStem = stem212
        , optionOrdinal = "c)"
        , optionText = Textarea "Consumes completely"
        , optionKey = True
        , optionPoints = 1
        }
        
    dis212_3 <- insert $ Option
        { optionStem = stem212
        , optionOrdinal = "d)"
        , optionText = Textarea "None of the mentioned"
        , optionKey = False
        , optionPoints = 0
        }

    r201 <- insert $ Exam
        { examTest = test201
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-3012) now
        , examEnd = pure $ addUTCTime (-3002) now
        }

    forM_ [ (stem201,key201)
          , (stem202,key202)
          , (stem203,key203)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,key207)
          , (stem208,key208)
          , (stem209,key209)
          , (stem210,dis210_1)
          , (stem211,key211)
          , (stem212,key212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r201
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r202 <- insert $ Exam
        { examTest = test201
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-4015) now
        , examEnd = pure $ addUTCTime (-4005) now
        }

    forM_ [ (stem201,dis201_1)
          , (stem202,dis201_2)
          , (stem203,dis203_3)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,key207)
          , (stem208,key208)
          , (stem209,key209)
          , (stem210,dis210_1)
          , (stem211,key211)
          , (stem212,key212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r202
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r203 <- insert $ Exam
        { examTest = test201
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-2015) now
        , examEnd = pure $ addUTCTime (-2005) now
        }

    forM_ [ (stem201,dis201_1)
          , (stem202,dis201_2)
          , (stem203,key203)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,key207)
          , (stem208,dis208_1)
          , (stem209,key209)
          , (stem210,key210)
          , (stem211,key211)
          , (stem212,key212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r203
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r204 <- insert $ Exam
        { examTest = test201
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-1013) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (stem201,key201)
          , (stem202,key202)
          , (stem203,key203)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,dis207_3)
          , (stem208,key208)
          , (stem209,key209)
          , (stem210,key210)
          , (stem211,key211)
          , (stem212,key212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r204
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r205 <- insert $ Exam
        { examTest = test201
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-1020) now
        , examEnd = pure $ addUTCTime (-1003) now
        }

    forM_ [ (stem201,key201)
          , (stem202,key202)
          , (stem203,dis203_2)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,dis207_3)
          , (stem208,key208)
          , (stem209,key209)
          , (stem210,dis210_3)
          , (stem211,key211)
          , (stem212,dis212_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r205
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r206 <- insert $ Exam
        { examTest = test201
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-2021) now
        , examEnd = pure $ addUTCTime (-2002) now
        }

    forM_ [ (stem201,key201)
          , (stem202,key202)
          , (stem203,key203)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,key207)
          , (stem208,key208)
          , (stem209,key209)
          , (stem210,dis210_3)
          , (stem211,key211)
          , (stem212,dis212_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r206
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r207 <- insert $ Exam
        { examTest = test201
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-5021) now
        , examEnd = pure $ addUTCTime (-5001) now
        }

    forM_ [ (stem201,dis201_2)
          , (stem202,key202)
          , (stem203,dis203_1)
          , (stem204,key204)
          , (stem205,dis205_3)
          , (stem206,key206)
          , (stem207,dis207_1)
          , (stem208,key208)
          , (stem209,dis209_2)
          , (stem210,key210)
          , (stem211,dis211_3)
          , (stem212,key212)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r207
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    skill301 <- insert $ Skill
                { skillCode = "SQL"
                , skillName = "SQL"
                , skillDescr = Just "Skills for writing SQL"
                }

    test301 <- insert $ Test
        { testCode = "E301"
        , testName = "SQL"
        , testDuration = 30
        , testPass = 9
        , testDescr = Just $ Textarea "SQL programming"
        , testState = TestStatePublished
        }

    stem301 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 1
               , stemText = Textarea "What is the full form of SQL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis301_1 <- insert $ Option
        { optionStem = stem301
        , optionOrdinal = "a."
        , optionText = Textarea "Structured Query List"
        , optionKey = False
        , optionPoints = 0
        }

    key301 <- insert $ Option
        { optionStem = stem301
        , optionOrdinal = "b."
        , optionText = Textarea "Structure Query Language"
        , optionKey = True
        , optionPoints = 1
        }

    dis301_2 <- insert $ Option
        { optionStem = stem301
        , optionOrdinal = "c."
        , optionText = Textarea "Sample Query Language"
        , optionKey = False
        , optionPoints = 0
        }

    dis301_3 <- insert $ Option
        { optionStem = stem301
        , optionOrdinal = "d."
        , optionText = Textarea "None of these"
        , optionKey = False
        , optionPoints = 0
        }

    stem302 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 2
               , stemText = Textarea "Which of the following is not a valid SQL type?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis302_1 <- insert $ Option
        { optionStem = stem302
        , optionOrdinal = "a."
        , optionText = Textarea "FLOAT"
        , optionKey = False
        , optionPoints = 0
        }

    dis302_2 <- insert $ Option
        { optionStem = stem302
        , optionOrdinal = "b."
        , optionText = Textarea "NUMERIC"
        , optionKey = False
        , optionPoints = 0
        }

    key302 <- insert $ Option
        { optionStem = stem302
        , optionOrdinal = "c."
        , optionText = Textarea "DECIMAL"
        , optionKey = True
        , optionPoints = 1
        }

    dis302_3 <- insert $ Option
        { optionStem = stem302
        , optionOrdinal = "d."
        , optionText = Textarea "CHARACTER"
        , optionKey = False
        , optionPoints = 0
        }

    stem303 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 3
               , stemText = Textarea "Which of the following is not a DDL command?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis303_1 <- insert $ Option
        { optionStem = stem303
        , optionOrdinal = "a."
        , optionText = Textarea "TRUNCATE"
        , optionKey = False
        , optionPoints = 0
        }

    dis303_2 <- insert $ Option
        { optionStem = stem303
        , optionOrdinal = "b."
        , optionText = Textarea "ALTER"
        , optionKey = False
        , optionPoints = 0
        }

    dis303_3 <- insert $ Option
        { optionStem = stem303
        , optionOrdinal = "c."
        , optionText = Textarea "CREATE"
        , optionKey = False
        , optionPoints = 0
        }

    key303 <- insert $ Option
        { optionStem = stem303
        , optionOrdinal = "d."
        , optionText = Textarea "UPDATE"
        , optionKey = True
        , optionPoints = 1
        }

    stem304 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 4
               , stemText = Textarea "Which of the following are TCL commands?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key304 <- insert $ Option
        { optionStem = stem304
        , optionOrdinal = "a."
        , optionText = Textarea "COMMIT and ROLLBACK"
        , optionKey = True
        , optionPoints = 1
        }

    dis304_1 <- insert $ Option
        { optionStem = stem304
        , optionOrdinal = "b."
        , optionText = Textarea "UPDATE and TRUNCATE"
        , optionKey = False
        , optionPoints = 0
        }

    dis304_2 <- insert $ Option
        { optionStem = stem304
        , optionOrdinal = "c."
        , optionText = Textarea "SELECT and INSERT"
        , optionKey = False
        , optionPoints = 0
        }

    dis304_3 <- insert $ Option
        { optionStem = stem304
        , optionOrdinal = "d."
        , optionText = Textarea "GRANT and REVOKE"
        , optionKey = False
        , optionPoints = 0
        }

    stem305 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 5
               , stemText = Textarea "Which statement is used to delete all rows in a table without having the action logged?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis305_1 <- insert $ Option
        { optionStem = stem305
        , optionOrdinal = "a."
        , optionText = Textarea "DELETE"
        , optionKey = False
        , optionPoints = 0
        }

    dis305_2 <- insert $ Option
        { optionStem = stem305
        , optionOrdinal = "b."
        , optionText = Textarea "REMOVE"
        , optionKey = False
        , optionPoints = 0
        }

    dis305_3 <- insert $ Option
        { optionStem = stem305
        , optionOrdinal = "c."
        , optionText = Textarea "DROP"
        , optionKey = False
        , optionPoints = 0
        }

    key305 <- insert $ Option
        { optionStem = stem305
        , optionOrdinal = "d."
        , optionText = Textarea "TRUNCATE"
        , optionKey = True
        , optionPoints = 1
        }

    stem306 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 6
               , stemText = Textarea "SQL Views are also known as"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis306_1 <- insert $ Option
        { optionStem = stem306
        , optionOrdinal = "a."
        , optionText = Textarea "Simple tables"
        , optionKey = False
        , optionPoints = 0
        }

    key306 <- insert $ Option
        { optionStem = stem306
        , optionOrdinal = "b."
        , optionText = Textarea "Virtual tables"
        , optionKey = True
        , optionPoints = 1
        }

    dis306_2 <- insert $ Option
        { optionStem = stem306
        , optionOrdinal = "c."
        , optionText = Textarea "Complex tables"
        , optionKey = False
        , optionPoints = 0
        }

    dis306_3 <- insert $ Option
        { optionStem = stem306
        , optionOrdinal = "d."
        , optionText = Textarea "Actual Tables"
        , optionKey = False
        , optionPoints = 0
        }

    stem307 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 7
               , stemText = Textarea "How many Primary keys can have in a table?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    key307 <- insert $ Option
        { optionStem = stem307
        , optionOrdinal = "a."
        , optionText = Textarea "Only 1"
        , optionKey = True
        , optionPoints = 1
        }

    dis307_1 <- insert $ Option
        { optionStem = stem307
        , optionOrdinal = "b."
        , optionText = Textarea "Only 2"
        , optionKey = False
        , optionPoints = 0
        }

    dis307_2 <- insert $ Option
        { optionStem = stem307
        , optionOrdinal = "c."
        , optionText = Textarea "Depends on no of Columns"
        , optionKey = False
        , optionPoints = 0
        }

    dis307_3 <- insert $ Option
        { optionStem = stem307
        , optionOrdinal = "d."
        , optionText = Textarea "Depends on DBA"
        , optionKey = False
        , optionPoints = 0
        }

    stem308 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 8
               , stemText = Textarea "Which datatype can store unstructured data in a column?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis308_1 <- insert $ Option
        { optionStem = stem308
        , optionOrdinal = "a."
        , optionText = Textarea "CHAR"
        , optionKey = False
        , optionPoints = 0
        }

    key308 <- insert $ Option
        { optionStem = stem308
        , optionOrdinal = "b."
        , optionText = Textarea "RAW"
        , optionKey = True
        , optionPoints = 1
        }

    dis308_2 <- insert $ Option
        { optionStem = stem308
        , optionOrdinal = "c."
        , optionText = Textarea "NUMERIC"
        , optionKey = False
        , optionPoints = 0
        }

    dis308_3 <- insert $ Option
        { optionStem = stem308
        , optionOrdinal = "d."
        , optionText = Textarea "VARCHAR"
        , optionKey = False
        , optionPoints = 0
        }

    stem309 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 9
               , stemText = Textarea "Which of the following is not Constraint in SQL?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis309_1 <- insert $ Option
        { optionStem = stem309
        , optionOrdinal = "a."
        , optionText = Textarea "Primary Key"
        , optionKey = False
        , optionPoints = 0
        }

    dis309_2 <- insert $ Option
        { optionStem = stem309
        , optionOrdinal = "b."
        , optionText = Textarea "Not Null"
        , optionKey = False
        , optionPoints = 0
        }

    dis309_3 <- insert $ Option
        { optionStem = stem309
        , optionOrdinal = "c."
        , optionText = Textarea "Check"
        , optionKey = False
        , optionPoints = 0
        }

    key309 <- insert $ Option
        { optionStem = stem309
        , optionOrdinal = "d."
        , optionText = Textarea "Union"
        , optionKey = True
        , optionPoints = 1
        }

    stem310 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 10
               , stemText = Textarea "Which of the following is not a valid aggregate function?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis310_1 <- insert $ Option
        { optionStem = stem310
        , optionOrdinal = "a."
        , optionText = Textarea "COUNT"
        , optionKey = False
        , optionPoints = 0
        }

    key310 <- insert $ Option
        { optionStem = stem310
        , optionOrdinal = "b."
        , optionText = Textarea "COMPUTE"
        , optionKey = True
        , optionPoints = 1
        }

    dis310_2 <- insert $ Option
        { optionStem = stem310
        , optionOrdinal = "c."
        , optionText = Textarea "SUM"
        , optionKey = False
        , optionPoints = 0
        }

    dis310_3 <- insert $ Option
        { optionStem = stem310
        , optionOrdinal = "d."
        , optionText = Textarea "MAX"
        , optionKey = False
        , optionPoints = 0
        }

    stem311 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 11
               , stemText = Textarea "Which data manipulation command is used to combines the records from one or more tables?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis311_1 <- insert $ Option
        { optionStem = stem311
        , optionOrdinal = "a."
        , optionText = Textarea "SELECT"
        , optionKey = False
        , optionPoints = 0
        }

    dis311_2 <- insert $ Option
        { optionStem = stem311
        , optionOrdinal = "b."
        , optionText = Textarea "PROJECT"
        , optionKey = False
        , optionPoints = 0
        }

    key311 <- insert $ Option
        { optionStem = stem311
        , optionOrdinal = "c."
        , optionText = Textarea "JOIN"
        , optionKey = True
        , optionPoints = 1
        }

    dis311_3 <- insert $ Option
        { optionStem = stem311
        , optionOrdinal = "d."
        , optionText = Textarea "PRODUCT"
        , optionKey = False
        , optionPoints = 0
        }

    stem312 <- insert $ Stem
               { stemTest = test301
               , stemSkill = skill301
               , stemOrdinal = 12
               , stemText = Textarea "Which operator is used to compare a value to a specified list of values?"
               , stemType = SingleRespose
               , stemInstruc = Textarea "Select one"
               }

    dis312_1 <- insert $ Option
        { optionStem = stem312
        , optionOrdinal = "a."
        , optionText = Textarea "ANY"
        , optionKey = False
        , optionPoints = 0
        }

    dis312_2 <- insert $ Option
        { optionStem = stem312
        , optionOrdinal = "b."
        , optionText = Textarea "BETWEEN"
        , optionKey = False
        , optionPoints = 0
        }

    dis312_3 <- insert $ Option
        { optionStem = stem312
        , optionOrdinal = "c."
        , optionText = Textarea "ALL"
        , optionKey = False
        , optionPoints = 0
        }

    key312 <- insert $ Option
        { optionStem = stem312
        , optionOrdinal = "d."
        , optionText = Textarea "IN"
        , optionKey = True
        , optionPoints = 1
        }

    r301 <- insert $ Exam
        { examTest = test301
        , examCandidate = c001
        , examAttempt = 1
        , examStart = addUTCTime (-4024) now
        , examEnd = pure $ addUTCTime (-4001) now
        }

    forM_ [ (stem301,dis301_2)
          , (stem302,key302)
          , (stem303,dis303_1)
          , (stem304,key304)
          , (stem305,dis305_3)
          , (stem306,key306)
          , (stem307,dis307_1)
          , (stem308,key308)
          , (stem309,dis309_2)
          , (stem310,key310)
          , (stem311,dis311_3)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r301
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r302 <- insert $ Exam
        { examTest = test301
        , examCandidate = c002
        , examAttempt = 1
        , examStart = addUTCTime (-3024) now
        , examEnd = pure $ addUTCTime (-3001) now
        }

    forM_ [ (stem301,key301)
          , (stem302,key302)
          , (stem303,dis303_1)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,dis307_1)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r302
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r303 <- insert $ Exam
        { examTest = test301
        , examCandidate = c003
        , examAttempt = 1
        , examStart = addUTCTime (-2024) now
        , examEnd = pure $ addUTCTime (-2001) now
        }

    forM_ [ (stem301,key301)
          , (stem302,key302)
          , (stem303,key303)
          , (stem304,dis304_2)
          , (stem305,key305)
          , (stem306,dis306_2)
          , (stem307,dis307_1)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r303
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r304 <- insert $ Exam
        { examTest = test301
        , examCandidate = c004
        , examAttempt = 1
        , examStart = addUTCTime (-1026) now
        , examEnd = pure $ addUTCTime (-1002) now
        }

    forM_ [ (stem301,dis301_1)
          , (stem302,key302)
          , (stem303,key303)
          , (stem304,dis304_2)
          , (stem305,key305)
          , (stem306,dis306_2)
          , (stem307,dis307_1)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,dis312_3)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r304
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r305 <- insert $ Exam
        { examTest = test301
        , examCandidate = c005
        , examAttempt = 1
        , examStart = addUTCTime (-926) now
        , examEnd = pure $ addUTCTime (-900) now
        }

    forM_ [ (stem301,key301)
          , (stem302,key302)
          , (stem303,key303)
          , (stem304,dis304_2)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r305
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r306 <- insert $ Exam
        { examTest = test301
        , examCandidate = c006
        , examAttempt = 1
        , examStart = addUTCTime (-823) now
        , examEnd = pure $ addUTCTime (-800) now
        }

    forM_ [ (stem301,key301)
          , (stem302,key302)
          , (stem303,key303)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r306
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r307 <- insert $ Exam
        { examTest = test301
        , examCandidate = c007
        , examAttempt = 1
        , examStart = addUTCTime (-720) now
        , examEnd = pure $ addUTCTime (-700) now
        }

    forM_ [ (stem301,dis301_3)
          , (stem302,key302)
          , (stem303,key303)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r307
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r308 <- insert $ Exam
        { examTest = test301
        , examCandidate = c008
        , examAttempt = 1
        , examStart = addUTCTime (-820) now
        , examEnd = pure $ addUTCTime (-803) now
        }

    forM_ [ (stem301,dis301_3)
          , (stem302,key302)
          , (stem303,dis303_2)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,key312)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r308
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r309 <- insert $ Exam
        { examTest = test301
        , examCandidate = c009
        , examAttempt = 1
        , examStart = addUTCTime (-920) now
        , examEnd = pure $ addUTCTime (-904) now
        }

    forM_ [ (stem301,dis301_3)
          , (stem302,key302)
          , (stem303,dis303_2)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,key306)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,dis312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r309
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r310 <- insert $ Exam
        { examTest = test301
        , examCandidate = c010
        , examAttempt = 1
        , examStart = addUTCTime (-1020) now
        , examEnd = pure $ addUTCTime (-1005) now
        }

    forM_ [ (stem301,dis301_3)
          , (stem302,key302)
          , (stem303,dis303_2)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,dis306_2)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,key309)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,dis312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r310
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    r311 <- insert $ Exam
        { examTest = test301
        , examCandidate = c011
        , examAttempt = 1
        , examStart = addUTCTime (-1112) now
        , examEnd = pure $ addUTCTime (-1100) now
        }

    forM_ [ (stem301,key301)
          , (stem302,key302)
          , (stem303,dis303_2)
          , (stem304,key304)
          , (stem305,key305)
          , (stem306,dis306_2)
          , (stem307,key307)
          , (stem308,key308)
          , (stem309,dis309_1)
          , (stem310,key310)
          , (stem311,key311)
          , (stem312,dis312_1)
          ] $ \(s,o) -> insert_ $ Answer { answerExam = r311
                                         , answerStem = s
                                         , answerOption = o
                                         , answerTime = now
                                         }

    return ()
