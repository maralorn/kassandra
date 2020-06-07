module Main (main) where
import Frontend
import Frontend.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
