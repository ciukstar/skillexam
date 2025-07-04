
[In english](https://github.com/ciukstar/skillexam/blob/master/README.md)  

[En français](https://github.com/ciukstar/skillexam/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/skillexam/blob/master/README.ro.md)  

# SkillExam

Приложение [«SkillExam»](https://skillexamru-i4rimw5qwq-de.a.run.app) предоставляет простой способ оценить навыки кандидата с помощью тестов с несколькими вариантами ответов.

## Обзор

Оцениваемые навыки должны быть определены в разделе [«Навыки»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/skills) в группе «Данные», доступной из главного меню. Навыки можно рассматривать как элементы группировки вопросов для экзамена.

Тесты для оценки навыков кандидата определяются в разделе [«Тесты»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/tests) в группе «Данные» главного меню. Тест состоит из вопросов.

Кандидаты на экзамен записываются в разделе [«Кандидаты»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/candidates) в группе «Данные» главного меню.

Кандидат может выбрать экзамен из списка [«Экзамены»](https://skillexamru-i4rimw5qwq-de.a.run.app), зарегистрироваться на экзамен и приступить к экзамену.

По завершению экзамена предоставляется сводка и кандидат может ознакомиться с результатами экзамена в разделе [«Мои экзамены»](https://skillexamru-i4rimw5qwq-de.a.run.app/my-exams).

## Основные сущности

### Навык

Навык записывается в разделе [«Навыки»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/skills) с уникальным кодом, названием и описанием.

### Кандидат

Кандидат записывается в раздел [«Кандидаты»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/candidates) с указанием своего полного имени и, по желанию, фотографии и дня рождения.

### Тест

Тест определяется в разделе [«Тесты»](https://skillexamru-i4rimw5qwq-de.a.run.app/admin/tests) путем предоставления уникального кода, имени, опционально описания, его продолжительности в минутах, проходного балла.

Рекомендуется, чтобы Тест оставался в состоянии «Неопубликованный» до тех пор, пока он не будет готов к публикации, путем определения и настройки всех его подкомпонентов: вопросов и вариантов ответов.

Тест состоит из нескольких вопросов.

Один Тест может охватывать несколько навыков, характерных для каждого вопроса.

### Вопрос

Вопрос для конкретного экзамена определяется путем предоставления порядкового номера, самого вопроса в виде текста, типа вопроса, инструкции и навыка, к которому он относится.

При необходимости текст Вопроса можно отформатировать с помощью ```HTML/CSS```.

Тип Вопроса может быть «Единичный ответ» или «Множественный ответ».

Для вопросов «Единичный ответ» кандидату предлагается выбрать только один ответ из предоставленных вариантов.

Для вопросов «Множественный ответ» кандидату предлагается выбрать все правильные ответы из предоставленных вариантов.

Для каждого Вопроса предусмотрено несколько вариантов ответа.

### Вариант

Вариант ответа вводится путем предоставления индекса (порядкового номера), ответа в виде текста, является ли ответ «ключём» или «отвлекающим», и значением для оценки.

При необходимости текст варианта ответа можно отформатировать с помощью ```HTML/CSS```.

После того, как все варианты ответов были предоставлены для всех вопросов экзамена, экзамен можно опубликовать.

Теперь кандидат может записаться на экзамен, выбрав экзамен из списка [«Экзамены»](https://skillexamru-i4rimw5qwq-de.a.run.app).

### Экзамен

Экзамен представляет собой конкретный экземпляр Теста для Кандидата.

Он описывает, когда экзамен начался и закончился для кандидата. Он также отслеживает количество попыток кандидата сдать один и тот же экзамен.

После регистрации кандидат приступает к экзамену, и его ответы записываются.

*Диаграмма конечного автомата*
![State Machine Diagram](static/img/SkillExam-SMD.svg)

По истечении времени экзамена система уведомляет кандидата и заставляет его завершить экзамен.

### Дистанционный экзамен

Администратор экзамена может создать URL-адрес для выбранного теста и, по желанию, для одного или нескольких зарегистрированных кандидатов.

Если при формировании ссылки на экзамен не выбран ни один кандидат, получателю ссылки будет предложено зарегистрироваться в качестве кандидата в системе. В этом случае регистрация пользователя не требуется.

### Ответ

Ответ представляет собой Вариант ответа для данного Вопроса, который Кандидат считает правильным.

## Диаграмма «Сущность-связь»

![Диаграмма Сущность-связь](static/img/SkillExam-ERD.svg)


## Демо

[Нажмите здесь, чтобы увидеть демо](https://skillexamru-i4rimw5qwq-de.a.run.app)
