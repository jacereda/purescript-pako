module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Traversable (for_)
import Pako (Level, WindowBits, MemLevel, Strategy, asBytes, byteSize, deflateText, deflateTextWithOptions, defaultOptions, inflateText)
import Test.QuickCheck (arbitrary, quickCheck', (<?>))
import Test.QuickCheck.Gen (randomSample')

newtype Res a = Res (Either Error a)

derive newtype instance showRes :: Show a => Show (Res a)

instance eqRes :: Eq a => Eq (Res a) where
  eq (Res (Right a)) (Res (Right b)) = a == b
  eq (Res (Left a)) (Res (Left b)) = true -- (eq `on` message) a b
  eq _ _ = false

assertEquals :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEquals expected actual = quickCheck' 1 $ actual == expected <?> msg
  where msg = show expected <> " /= " <> show actual


assertEffEquals :: forall a. Eq a => Show a => a -> Effect a -> Effect Unit
assertEffEquals expectedValue computation = do
  actualValue <- computation
  let msg = show expectedValue <> " /= " <> show actualValue
  quickCheck' 1 $ actualValue == expectedValue <?> msg


main :: Effect Unit
main = do
  for_ (enumFromTo bottom top :: Array Level) \level ->
    for_ (enumFromTo bottom top :: Array WindowBits) \windowBits ->
      for_ (enumFromTo bottom top :: Array MemLevel) \memLevel ->
        for_ (enumFromTo bottom top :: Array Strategy) \strategy ->
          chk level windowBits memLevel strategy json
  xs <- randomSample' 100 arbitrary
  for_ xs \x -> do
    assertEquals (Res (Right x)) $ Res $ join $ inflateText <$> deflateText x
    assertEquals (Res (Left (error "incorrect header check"))) $ Res $ inflateText $ asBytes x
  where chk :: Level -> WindowBits -> MemLevel -> Strategy -> String -> Effect Unit
        chk level windowBits memLevel strategy x = do
          let def = deflateTextWithOptions (defaultOptions { level = level
                                                           , windowBits = windowBits
                                                           , memLevel = memLevel
                                                           , strategy = strategy
                                                           }) x
          log $ show level <> " "
            <> show windowBits <> " "
            <> show memLevel <> " "
            <> show strategy <> " "
            <> case def of
              Right b -> show $ byteSize b
              Left e -> show e
        json = """
{"result":[{"xys":[[62,1191089],[65,1239181],[68,1264155],[71,1287547],[75,1489468],[79,1522502],[83,1593357],[87,1701892],[91,1652430],[96,1704406],[101,1910909],[106,2021406],[111,2016603],[117,2263775],[123,2264600],[129,2397493],[135,2526666],[142,2582700],[149,2783401],[156,3434092],[164,3230944],[172,3094147],[181,3397486],[190,3487876],[200,3595016],[210,3965509],[221,3780338],[232,3978115],[244,4123473],[256,4296993],[269,4707701],[282,4797375],[296,5754376],[311,5139904],[327,5656406],[343,5619739],[360,6111764],[378,6340549],[397,6827950],[417,6986528],[438,7499981],[460,13101993],[483,8248526],[507,8240519]],"r":0.9475175653495644,"name":"sum/1000int","b":18075.23569810084,"a":27565.18656867539},{"xys":[[28,2696728],[29,2168920],[30,2875591],[32,3069856],[34,3119173],[36,3915701],[38,3499310],[40,3530549],[42,3680819],[44,5644460],[46,4711363],[48,5473500],[50,4873401],[53,5270089],[56,4715762],[59,5538919],[62,5772335],[65,7956161],[68,7869843],[71,7036286],[75,6762558],[79,9338895],[83,9729181],[87,8102358],[91,8305316],[96,9383167],[101,10002885],[106,9728944]],"r":0.9559928647438682,"name":"sum/2000int","b":100327.48576909404,"a":-23855.501187003083},{"xys":[[38,2331758],[40,2685534],[42,2353246],[44,1813990],[46,2487347],[48,2657603],[50,2700333],[53,2249300],[56,2932861],[59,3916293],[62,4384092],[65,4549952],[68,3967771],[71,3760730],[75,3810910],[79,4557691],[83,4150280],[87,4266680],[91,5243722],[96,4799451],[101,5596725],[106,5271354],[111,6001746],[117,6265650],[123,6568330],[129,6255565],[135,6541313],[142,6887727],[149,7261091],[156,8436394],[164,8287135],[172,9459849],[181,9022902],[190,9526461]],"r":0.9815315476830402,"name":"foldl/1000int","b":48267.55872961209,"a":445465.8488847815},{"xys":[[28,2594421],[29,2484810],[30,3091689],[32,3032182],[34,3308634],[36,4018389],[38,3505473],[40,3705352],[42,3836577],[44,4672148],[46,4908553],[48,5478657],[50,4417464],[53,5418362],[56,4850298],[59,5715696],[62,5912367],[65,6218475],[68,6396445],[71,7312413],[75,7530431],[79,8369562],[83,9363612],[87,10992692],[91,9752551],[96,9643705],[101,10026371],[106,9607109]],"r":0.9713098746529044,"name":"foldl/2000int","b":106105.25892876316,"a":-314397.6419118015},{"xys":[[65,1109611],[68,1223253],[71,1207828],[75,1276097],[79,1416114],[83,1387015],[87,1442432],[91,1675666],[96,1724315],[101,1781056],[106,1775160],[111,1900186],[117,1927380],[123,2101277],[129,2277341],[135,2241090],[142,2286394],[149,2371139],[156,2717865],[164,2883558],[172,2829876],[181,3191486],[190,3278982],[200,3461756],[210,3608804],[221,3835656],[232,3859932],[244,4122448],[256,4217884],[269,4468881],[282,4628349],[296,4894257],[311,5077169],[327,5412307],[343,5539692],[360,6006934],[378,6307194],[397,6551877],[417,7037718],[438,7620902],[460,7474059],[483,8643163],[507,8835343],[532,9080072],[559,9291432]],"r":0.9983753411138413,"name":"foldr/1000int","b":16848.65841948405,"a":12352.66395361291},{"xys":[[48,1757751],[50,1827757],[53,1705109],[56,1884895],[59,1903510],[62,1999921],[65,2211843],[68,2220864],[71,2389201],[75,2746562],[79,2601871],[83,2746183],[87,2783298],[91,2926621],[96,3157275],[101,3421443],[106,3678061],[111,3799127],[117,4012204],[123,4165664],[129,4336415],[135,4461596],[142,4609574],[149,4850748],[156,5141688],[164,5320400],[172,5560621],[181,5925183],[190,6052622],[200,6587455],[210,6782978],[221,7283777],[232,8340686],[244,8503005],[256,8997637],[269,9338659],[282,9539666],[296,10141907]],"r":0.9974330542773725,"name":"foldr/2000int","b":34226.12639930561,"a":-85648.36689392202}],"name":"Int"}
"""
