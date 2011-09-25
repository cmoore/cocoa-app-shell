
module HSTypes where

import qualified Data.Map as M
import HSObjC
import qualified Data.Text as T
import Data.IORef
import Control.Monad.Error

type OutletTable = M.Map T.Text StableId                     

data Controller = Controller { 
                    ctrOutlets :: OutletTable,
                    ctrMethods :: MethodTable,                    
                    ctrModel   :: Model
                  }

type MethodTable = M.Map T.Text (Controller -> IOOBJC StableId)


type Answer = [(Int, T.Text)]

data Model = Model { mdSimpleName    :: IORef T.Text
                   , mdStringsLength :: IORef Answer
                   , mdCheckTransaction :: IORef Bool
                   }

outlet' :: Controller -> String -> IOOBJC StableId
outlet' cn on =
  case (T.pack on) `M.lookup` (ctrOutlets cn) of
    Nothing -> throwError $ "Could not find the outlet named: " ++ on
    Just x -> return x
