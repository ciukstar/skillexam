
[En français](https://github.com/ciukstar/skillexam/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/skillexam/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/skillexam/blob/master/README.ru.md)

# SkillExam

The application ["SkillExam"](https://skillexam-i4rimw5qwq-de.a.run.app) provides an easy way to assess a candidate's skills with Multiple-Choice Questions tests.

## Overview

The skills to be assessed must be defined in the ["Skills"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/skills) section in the "Data" group, accessible from the main menu. Skills can be thought of as elements for grouping questions for an exam.

Tests to assess a candidate's skills are defined in the ["Tests"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/tests) section in the group "Data" of the main menu. A Test consists of questions.

Candidates for the exam are recorded in the ["Candidates"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/candidates) section in the "Data" group of the main menu.

A candidate can choose an exam from the ["Exams"](https://skillexam-i4rimw5qwq-de.a.run.app) list, register for the exam and start the exam.

Upon completion of the exam, a summary is provided and the candidate can see the results of the exam in the ["My exams"](https://skillexam-i4rimw5qwq-de.a.run.app/my-exams) section.

## Basic Entities

### Skill
A Skill is recorded in the ["Skills"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/skills) section with a unique code, name and description.

### Candidate
A Candidate is registered in the ["Candidates"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/candidates) section by providing its full name and, optionally, a photo and birthday.

### Test
A Test is defined in the ["Tests"](https://skillexam-i4rimw5qwq-de.a.run.app/admin/tests) section by providing a unique code, a name, optionally a description, its duration in minutes, and a passing score.

It is recommended that a Test remain in the "Unpublished" state until it is ready for publication by defining and configuring all of its subcomponents: Questions and Answer Options.

A Test is composed from multiple Questions.

A single Test may cover several Skills specific to each Question.

### Question
A Question (Stem) for a particular test is defined by providing an ordinal number, the question itself as text, the question type, the instruction, and the Skill it addresses.

If necessary, the text of the Question can be formatted with ```HTML/CSS```.

The type of a Question may be one of "Single response" or "Multiple response".

For "Single response" questions, the candidate is asked to select only one answer from the options provided.

For "Multiple response" questions, the candidate is asked to select all of the answers that are correct from the options provided.

For each Question, several answer Options are provided.

### Option

A response Option is entered by providing an index (ordinal), a response as text, whether the response is a "key" or a "distractor", and a value for the score.

If necessary, the text of the response Option can be formatted with ```HTML/CSS```.

After all Options have been provided for all Questions of a Test, the Test can be published.

A Candidate can now enroll for an exam by choosing a test from the list ["Exams"](https://skillexam-i4rimw5qwq-de.a.run.app).

### Exam

An Exam represents a particular instance of a Test for a Candidate.

It describes when the Exam started and ended for the Candidate. It also tracks the number of attempts a Candidate tries to pass the same Test.

After enrollment the candidate starts the Exam and its Answers are recorded.

*State Machine Diagram*
![State Machine Diagram](static/img/SkillExam-SMD.svg)

Once the exam time has expired, the system notifies the candidate and forces him to finish the exam.

### Remote exam

The exam administrator can generate a URL for a selected test and, optionally, for one or more registered candidates.

If no candidate is selected when generating the exam link, the link recipient will be prompted to register as a candidate in the system. In this case, user registration is not required.

### Answer

An Answer represents the answer Option for a given Question that a Candidate considers to be the correct one.

## Entity Relationship Diagram

![Entity Relationship Diagram](static/img/SkillExam-ERD.svg)
 
## Demo

[Click here to see demo](https://skillexam-i4rimw5qwq-de.a.run.app)
