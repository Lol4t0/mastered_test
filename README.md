# Mastered test

Тест работы нескольких обработчиков для синхронизации которых использутся система zookeeper.


## Установка

### Сборка

* Клонируем [ezk](https://github.com/infinipool/ezk) (предоставялющую API Zookeeper для Erlang).

* Собираем ezk

`cd ezk && make && cd ..`

* Собираем mastered_test

`./rebar compile`

### Настройка конфигурации

* Устанавливаем количество обработчиков
* Устанавливаем приоритеты обработчиков
* Задааем имя сервера

`ebin/mastered_test.app`

* `orders` : список приоритетов обработчиков _на данной ноде_.
    То есть `[1,35]` ознаечает, что первый обработчик имеет приоритет `1`, второй - `35`

* `number_of_workers` : число обработчиков

* address : строка адреса. Используется для идентификации ноды при общении с клиентом.

### Настройка zookeeper

* Устанавливаем

* Создаем пути для проведения голосований обработчиками (например, через zkCli.sh)

`/mastered_test/election/workerN`, где `N=1,,Число обработчиков`.

## Использование

Для удобства управления выделен фасад mtf.

* Запуск обработчиков:

  `mtf:start()`

* Остановка обработчиков:

  `mtf:stop()`

* Запрос к обработчику

  `mtf:ask(N)`, где `N` - номер обработчика.

  Если обрабочик на ноде, с которой происходит запрос, имеет наивысший приоритет
  из всех _запущенных_ нод (наименьший номер Order), то он отвечает непосредственно.

  В противном случае он сообщеает _адрес_ ноды, у которой нужно спросить данные
  (при этом та нода может произвести перенаправление дальше по цепочке увеличения приоритета)

