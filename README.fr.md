
[In english](https://github.com/ciukstar/skillexam/blob/master/README.md)  

[În română](https://github.com/ciukstar/skillexam/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/skillexam/blob/master/README.ru.md)

# SkillExam

L'application [«SkillExam»](https://skillexamfr-i4rimw5qwq-de.a.run.app) offre un moyen simple d'évaluer les compétences d'un candidat au moyen de questions à choix multiples.

## Aperçu

Les compétences à évaluer doivent être définies dans la rubrique [«Compétences»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/skills) du groupe «Données», accessible depuis le menu principal. Les compétences peuvent être considérées comme des éléments permettant de regrouper les questions d'un test.

Les tests permettant d'évaluer les compétences d'un candidat sont définis dans la section [«Tests»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/tests) du groupe «Données» du menu principal. Un test est composé de questions.

Les candidats à l'examen sont enregistrés dans la section [«Candidats»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/candidates) du groupe «Données» du menu principal.

Un candidat peut choisir un examen dans la liste [«Examens»](https://skillexamfr-i4rimw5qwq-de.a.run.app), s'inscrire à l'examen et commencer l'examen.

À la fin de l'examen, un résumé est fourni et le candidat peut voir les résultats de l'examen dans la section [«Mes examens»](https://skillexamfr-i4rimw5qwq-de.a.run.app/my-exams).

## Entités de base

### Compétence

Une Compétence est enregistrée dans la section [«Compétences»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/skills) avec un code unique, un nom et une description.

### Candidat

Un Candidat est enregistré dans la rubrique [«Candidats»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/candidates) en fournissant son nom complet et éventuellement une photo et sa date de naissance.

### Tests

Un Test est défini dans la rubrique [«Tests»](https://skillexamfr-i4rimw5qwq-de.a.run.app/admin/tests) en fournissant un code unique, un nom, éventuellement une description, sa durée en minutes, sa note de passage.

Il est recommandé qu'un test reste à l'état «Non publié» jusqu'à ce qu'il soit prêt à être publié en définissant et en configurant tous ses sous-composants: questions et options de réponse.

Un test est composé de plusieurs questions.

Un même test peut couvrir plusieurs compétences spécifiques à chaque question.

### Question

Une question pour un test particulier est définie en fournissant un nombre ordinal, la question elle-même sous forme de texte, le type de question, l'instruction et la compétence qu'elle aborde.

Si nécessaire, le texte de la question peut être formaté avec ```HTML/CSS```.

Le type d'une Question peut être «Réponse unique» ou «Réponse multiple».

Pour les questions à «Réponse unique», le candidat est invité à sélectionner une seule réponse parmi les options proposées.

Pour les questions à «Réponse multiple», le candidat est invité à sélectionner toutes les réponses correctes parmi les options proposées.

Pour chaque question, plusieurs options de réponse sont proposées.

### Option

Une option de réponse est entrée en fournissant un index (numéro de séquence), une réponse textuelle, si la réponse est une «clé» ou une «distraction», et une valeur de score.

Si nécessaire, le texte de l'option de réponse peut être formaté avec ```HTML/CSS```.

Une fois que toutes les options de réponse ont été fournies pour toutes les questions d'un test, le test peut être publié.

Un candidat peut désormais s'inscrire à un examen en choisissant un examen dans la liste ["Examens"](https://skillexamfr-i4rimw5qwq-de.a.run.app).

### Examen

L'Examen représente une instance particulière d'un test pour un candidat.

Il décrit quand l'examen a commencé et s'est terminé pour un candidat. Il suit également le nombre de tentatives d'un candidat pour réussir le même test.

Après l'inscription, le candidat commence l'examen et ses réponses sont enregistrées.

*Diagramme de machine d'état*
![State Machine Diagram](static/img/SkillExam-SMD.svg)

Une fois le temps d'examen expiré, le système avertit le candidat et l'oblige à terminer l'examen.


### Examen à distance

L'administrateur de l'examen peut générer une URL pour un test sélectionné et, éventuellement, pour un ou plusieurs candidats inscrits.

Si aucun candidat n'est sélectionné lors de la génération du lien d'examen, le destinataire du lien sera invité à s'inscrire comme candidat dans le système. Dans ce cas, l'inscription de l'utilisateur n'est pas requise.


### Réponse

Une réponse représente l'option pour une question donnée qu'un candidat considère comme la bonne.

## Diagramme entité-relation

![Diagramme entité-relation](static/img/SkillExam-ERD.svg)
 
## Démo

[Cliquez ici pour voir la démo](https://skillexamfr-i4rimw5qwq-de.a.run.app)
