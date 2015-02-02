-- Модуль для тестирования библиотеки


import Text.Parsec

import qualified Address.Digit as D
import qualified Address.Symbol as S
import Address.Main


main = do

    -- TODO: Тут хорошо было бы сделать модульное тестирование

    --parseTest D.prefix "строение 1-A/2"
    --parseTest D.prefix "стр 1-A/2"
    --parseTest D.prefix "стр. 1-A/2"
    --parseTest D.prefix "стр.1-A/2"
    --parseTest D.postfix "1-A/2 строение"
    --parseTest D.postfix "1-A/2 стр"

    --parseTest S.prefix "ул. 1-я Дубровская"

    --parseTest address "д.1 к.2"
    --parseTest address "ул. Дубровская д.4 к.2"
    --parseTest address "ул. 1-я Дубровская, д.4"
    --parseTest address "г. Москва ул. 1-я Дубровская д.4 к.2"

    --parseTest address "Раменское г. Высоковольтная ул"
    --parseTest address "город москва улица дубровская"
    --parseTest address "Егорьевск г. Профсоюзная ул. д. 30" -- Без запятых
    --parseTest address "Москва г."
    --parseTest address "г Москва"
    --parseTest address "Всеволода Вишневского ул., д.4-А"
    --parseTest address "ул. Всеволода Вишневского, д.4-А"
    --parseTest address "Ореховый б-р, д.49, к.1"
    --parseTest address "Семеновская наб., д.3, к.1"
    --parseTest address "Алябьева ул.,д.1-33,к.1"
    --parseTest address "Алябьева УЛ.,Д.1-33,к.1"
    --parseTest address "Зеленоград г., КОРПУС №225-А"

    --parseTest address "пр. Ленинский, д.10, к.5"
    --parseTest address "д.1, к.2, Ленинский п."
    --parseTest address "Ленинский пр, д.10, к.5"
    --parseTest address "ул. 1-я Дубровская, д.4"
    --parseTest address "Алябьева ул., д.7/33, к.1"

    print $ parseAddr "ул. Таганская, д.1, к.1"
    print $ parseAddr "дом 1, ул. Таганская, корпус 1"
