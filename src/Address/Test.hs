-- Модуль для тестирования библиотеки


import Text.Parsec

import qualified Address.Digit as D
import qualified Address.Symbol as S
import Address.Main
import Address.Types


main = do

    -- TODO: Тут хорошо было бы сделать модульное тестирование

    {-
    print $ parseAddr "строение 1-A/2"
    print $ parseAddr "стр 1-A/2"
    print $ parseAddr "стр. 1-A/2"
    print $ parseAddr "стр.1-A/2"
    print $ parseAddr "1-A/2 строение"
    print $ parseAddr "1-A/2 стр"
    print $ parseAddr "1-A/2 с"

    print $ parseAddr "ул. 1-я Дубровская"

    print $ parseAddr "д.1 к.2"
    print $ parseAddr "ул. Дубровская д.4 к.2"
    print $ parseAddr "ул. 1-я Дубровская, д.4"
    print $ parseAddr "г. Москва ул. 1-я Дубровская д.4 к.2"

    print $ parseAddr "Раменское г. Высоковольтная ул"
    print $ parseAddr "город москва улица дубровская"
    print $ parseAddr "Егорьевск г. Профсоюзная ул. д. 30" -- Без запятых
    print $ parseAddr "Москва г."
    print $ parseAddr "г Москва"
    print $ parseAddr "Всеволода Вишневского ул., д.4-А"
    print $ parseAddr "ул. Всеволода Вишневского, д.4-А"
    print $ parseAddr "Ореховый б-р, д.49, к.1"
    print $ parseAddr "Семеновская наб., д.3, к.1"
    print $ parseAddr "Алябьева ул.,д.1-33,к.1"
    print $ parseAddr "Алябьева УЛ.,Д.1-33,к.1"
    print $ parseAddr "Зеленоград г., КОРПУС №225-А"

    print $ parseAddr "пр. Ленинский, д.10, к.5"
    print $ parseAddr "д.1, к.2, Ленинский пр."
    print $ parseAddr "Ленинский б-р, д.10, к.5"
    print $ parseAddr "ул. 1-я Дубровская, д.4"
    print $ parseAddr "Алябьева ул., д.7/33, к.1"

    print $ parseAddr "МО, ул. 1-я Дубровская"
    -}

    -- Без ключей
    --print $ parseAddr "Таганская, 1"
    --print $ parseAddr "1 Дубровская, 1"
    --print $ parseAddr "1, 1, Дубровская"

    --print $ parseAddr "Вернадского пр-т.,д.28 (3)"
    print $ parseAddr "Одинцово г .,Любы Новоселовой б-р,д.17"

    {-
    print $ parseAddr "МО, 1-я Дубровская шоссе ул, д1, лит А"
    print $ parseAddr "Волхонское шоссе ул,д.31"
    print $ parseAddr "Мытнинская ул.,д.25 Лит. А"
    print $ parseAddr "Обводного канала набережная ул.,д.123 Лит А"
    print $ parseAddr "Серебристый бульвар ул.,д.14 к.2"
    print $ parseAddr "Артюхиной ул, д.27 к.3 (3)"
    -}

    --putStrLn $ format $ fromRight $ parseAddr "МО, ул. 1-я Дубровская, д.99"
    --where fromRight (Right x) = x
