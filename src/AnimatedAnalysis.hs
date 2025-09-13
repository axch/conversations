module AnimatedAnalysis (display) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | Represents an equation in the analysis with its state and formatting
data Equation = Equation
  { eqBinding :: String
  , eqValue :: String
  , eqVisible :: Bool
  , eqActionable :: Bool
  , eqNewlyCreated :: Bool
  , eqNewlyUpdated :: Bool
  } deriving (Show, Eq)

-- | Represents a binding with its program point and possible values
data Binding = Binding
  { bindPoint :: String
  , bindValues :: [String]
  , bindAffects :: [String]
  } deriving (Show)

-- | Analysis state containing equations and metadata
data State = State
  { stateData :: M.Map String Binding
  , stateResult :: [Equation]
  , stateResIndexes :: M.Map String Int
  , stateContext :: Maybe String
  , stateLastName :: Maybe String
  } deriving (Show)

-- | Commands that can be executed to modify the analysis state
data Command 
  = Create String
  | Act String Command
  | Act2 String Command Command
  | Evaluate (Maybe String)
  | Hiding [String] Command
  | Depends String
  deriving (Show)

-- | Create a new equation with the given binding
mkEquation :: String -> Bool -> Equation
mkEquation binding newlyCreated = Equation
  { eqBinding = binding
  , eqValue = "\\bot"
  , eqVisible = True
  , eqActionable = True
  , eqNewlyCreated = newlyCreated
  , eqNewlyUpdated = False
  }

-- | Update an equation's value and newly_updated flag
updateEquation :: Equation -> String -> Bool -> Equation
updateEquation eq newValue newlyUpdated = eq
  { eqValue = newValue
  , eqActionable = False
  , eqNewlyUpdated = newlyUpdated
  }

-- | Render an equation as LaTeX
renderEquation :: Equation -> String
renderEquation eq = bindingR <> " & " <> mapstoR <> " & " <> valueR
  where
    bindingR
      | eqActionable eq = "\\bbox[color:orange]{" <> eqBinding eq <> "}"
      | eqNewlyCreated eq = "\\bbox[color:green]{" <> eqBinding eq <> "}"
      | otherwise = eqBinding eq
    
    mapstoR
      | eqNewlyCreated eq = "\\bbox[color:green]{\\mapsto}"
      | otherwise = "\\mapsto"
    
    valueR
      | eqNewlyCreated eq || eqNewlyUpdated eq = "\\bbox[color:green]{" <> eqValue eq <> "}"
      | otherwise = eqValue eq

-- | Initial data bindings (from Ruby @data hash)
initialData :: M.Map String Binding
initialData = M.fromList
  [ ("mandel_expr", Binding "\\exp{(iterate (real 400) (step c) c:0)}, \\env{1}" ["\\RR"] [])
  , ("iterate_var1", Binding "\\exp{iterate}, \\env{1}" ["\\obj{iterate}"] [])
  , ("real_expr", Binding "\\exp{(real 400)}, \\env{1}" ["\\R"] [])
  , ("real_var", Binding "\\exp{real}, \\env{1}" ["\\obj{real}"] [])
  , ("real_app", Binding "\\obj{real}, 400" ["\\R"] [])
  , ("step_c_expr", Binding "\\exp{(step c)}, \\env{1}" ["\\obj{closure1}"] [])
  , ("step_var", Binding "\\exp{step}, \\env{1}" ["\\obj{step}"] [])
  , ("c_var", Binding "\\exp{c}, \\env{1}" ["\\RR"] [])
  , ("step_app", Binding "\\obj{step}, \\RR" ["\\obj{closure1}"] [])
  , ("czero_var", Binding "\\exp{c:0}, \\env{1}" ["\\RR"] [])
  , ("iterate_app", Binding "\\obj{iterate}, \\R, \\obj{closure1}, \\RR" ["\\RR"] [])
  , ("if_expr", Binding "\\exp{(if ...)}, \\env{2}" ["\\RR", "\\RR"] [])
  , ("compare_expr", Binding "\\exp{(<= count 0)}, \\env{2}" ["\\B"] [])
  , ("count_var", Binding "\\exp{count}, \\env{2}" ["\\R"] [])
  , ("compare_app", Binding "\\obj{<=}, \\R, 0" ["\\B"] [])
  , ("x_var", Binding "\\exp{x}, \\env{2}" ["\\RR"] [])
  , ("iterate_expr", Binding "\\exp{(iterate (- count 1) f (f x))}, \\env{2}" ["\\RR"] [])
  , ("iterate_var2", Binding "\\exp{iterate}, \\env{2}" ["\\obj{iterate}"] [])
  , ("sub_expr", Binding "\\exp{(- count 1)}, \\env{2}" ["\\R"] [])
  , ("f_var", Binding "\\exp{f}, \\env{2}" ["\\obj{closure1}"] [])
  , ("fx_exp", Binding "\\exp{(f x)}, \\env{2}" ["\\RR"] [])
  , ("closure_app", Binding "\\obj{closure1}, \\RR" ["\\RR"] [])
  ]

-- | Command sequence (from Ruby @commands array)
commands :: [Command]
commands = 
  [ Create "mandel_expr"
  , Act "mandel_expr" (Create "iterate_var1")
  , Evaluate Nothing
  , Act "mandel_expr" (Create "real_expr")
  , Act "real_expr" (Create "real_var")
  , Evaluate Nothing
  , Act "real_expr" (Create "real_app")
  , Evaluate Nothing
  , Evaluate (Just "real_expr")
  , Hiding ["real_var", "real_app"] (Act "mandel_expr" (Create "step_c_expr"))
  , Act "step_c_expr" (Create "step_var")
  , Evaluate Nothing
  , Act "step_c_expr" (Create "c_var")
  , Evaluate Nothing
  , Act "step_c_expr" (Create "step_app")
  , Evaluate Nothing
  , Evaluate (Just "step_c_expr")
  , Hiding ["step_var", "c_var", "step_app"] (Act "mandel_expr" (Create "czero_var"))
  , Evaluate Nothing
  , Act "mandel_expr" (Create "iterate_app")
  , Hiding ["iterate_var1", "real_expr", "step_c_expr", "czero_var"] (Act "iterate_app" (Create "if_expr"))
  , Act "if_expr" (Create "compare_expr")
  , Act "compare_expr" (Create "count_var")
  , Evaluate Nothing
  , Act "compare_expr" (Create "compare_app")
  , Evaluate Nothing
  , Evaluate (Just "compare_expr")
  , Hiding ["count_var", "compare_app"] (Act2 "if_expr" (Create "x_var") (Create "iterate_expr"))
  , Evaluate (Just "x_var")
  , Act "iterate_expr" (Create "iterate_var2")
  , Evaluate Nothing
  , Act "iterate_expr" (Create "sub_expr")
  , Evaluate Nothing
  , Act "iterate_expr" (Create "f_var")
  , Evaluate Nothing
  , Act "iterate_expr" (Create "fx_exp")
  , Act "fx_exp" (Create "closure_app")
  , Evaluate Nothing
  , Evaluate (Just "fx_exp")
  , Hiding ["closure_app"] (Act "iterate_expr" (Depends "iterate_app"))
  , Hiding ["iterate_var2", "sub_expr", "f_var", "fx_exp"] (Evaluate (Just "if_expr"))
  , Evaluate (Just "iterate_app")
  , Evaluate (Just "iterate_expr")
  , Evaluate (Just "if_expr")
  , Evaluate (Just "mandel_expr")
  ]

-- | Initial empty state
initialState :: State
initialState = State
  { stateData = initialData
  , stateResult = []
  , stateResIndexes = M.empty
  , stateContext = Nothing
  , stateLastName = Nothing
  }

-- | Execute a single command on the state
executeCommand :: Bool -> State -> Command -> State
executeCommand firstLight state cmd = case cmd of
  Create name -> createBinding firstLight name state
  Act name subCmd -> actOnBinding firstLight name subCmd state  
  Act2 name cmd1 cmd2 -> actOnBinding firstLight name cmd2 (actOnBinding firstLight name cmd1 state)
  Evaluate mName -> evaluateBinding firstLight (fromMaybe (fromMaybe "" (stateLastName state)) mName) state
  Hiding names subCmd -> hideBindings names (executeCommand firstLight state subCmd)
  Depends name -> dependsOnContext name state

-- | Create a new binding equation
createBinding :: Bool -> String -> State -> State
createBinding firstLight name state = 
  let binding = M.findWithDefault (Binding "" [] []) name (stateData state)
      newEq = mkEquation (bindPoint binding) firstLight
      newIndex = length (stateResult state)
      newState = state
        { stateResult = stateResult state ++ [newEq]
        , stateResIndexes = M.insert name newIndex (stateResIndexes state)
        , stateLastName = Just name
        }
  in case stateContext state of
       Just ctx -> addDependency name ctx newState
       Nothing -> newState

-- | Act on a binding (make it non-actionable and execute subcommand)
actOnBinding :: Bool -> String -> Command -> State -> State
actOnBinding firstLight name subCmd state =
  let maybeIdx = M.lookup name (stateResIndexes state)
  in case maybeIdx of
       Just idx -> 
         let oldEq = stateResult state !! idx
             newEq = oldEq { eqActionable = False }
             newResult = take idx (stateResult state) ++ [newEq] ++ drop (idx + 1) (stateResult state)
             newState = state { stateResult = newResult, stateContext = Just name }
             finalState = executeCommand firstLight newState subCmd
         in finalState { stateContext = Nothing }
       Nothing -> state

-- | Evaluate a binding (update its value)
evaluateBinding :: Bool -> String -> State -> State  
evaluateBinding firstLight name state =
  let maybeIdx = M.lookup name (stateResIndexes state)
      maybeBinding = M.lookup name (stateData state)
  in case (maybeIdx, maybeBinding) of
       (Just idx, Just binding) ->
         let oldEq = stateResult state !! idx
             oldValue = eqValue oldEq
             newValue = case bindValues binding of
                          (v:_) -> v
                          [] -> oldValue
             newEq = if oldValue /= newValue
                     then updateEquation oldEq newValue firstLight
                     else oldEq { eqActionable = False }
             newResult = take idx (stateResult state) ++ [newEq] ++ drop (idx + 1) (stateResult state)
             -- Update affected bindings to be actionable if value changed
             newResult2 = if oldValue /= newValue
                         then makeAffectedActionable (bindAffects binding) state newResult
                         else newResult
             -- Remove first value from binding's values list
             updatedBinding = binding { bindValues = drop 1 (bindValues binding) }
             newData = M.insert name updatedBinding (stateData state)
         in state { stateResult = newResult2, stateData = newData }
       _ -> state

-- | Make affected bindings actionable
makeAffectedActionable :: [String] -> State -> [Equation] -> [Equation]
makeAffectedActionable affects state result = 
  foldr (\name acc -> case M.lookup name (stateResIndexes state) of
                        Just idx | idx < length acc -> 
                          let eq = acc !! idx
                              newEq = eq { eqActionable = True }
                          in take idx acc ++ [newEq] ++ drop (idx + 1) acc
                        _ -> acc) result affects

-- | Hide specified bindings
hideBindings :: [String] -> State -> State
hideBindings names state =
  let newResult = map (\eq -> 
        if any (\name -> case M.lookup name (stateResIndexes state) of
                           Just idx -> stateResult state !! idx == eq
                           Nothing -> False) names
        then eq { eqVisible = False }
        else eq) (stateResult state)
  in state { stateResult = newResult }

-- | Add dependency relationship
addDependency :: String -> String -> State -> State
addDependency name ctxName state =
  case M.lookup ctxName (stateData state) of
    Just binding -> 
      let updatedBinding = binding { bindAffects = bindAffects binding ++ [name] }
          newData = M.insert ctxName updatedBinding (stateData state)
      in state { stateData = newData }
    Nothing -> state

-- | Add dependency (used by Depends command)
dependsOnContext :: String -> State -> State  
dependsOnContext name state = case stateContext state of
  Just ctx -> addDependency ctx name state
  Nothing -> state

-- | Execute commands up to step i and return final state
computeState :: Int -> State
computeState i = 
  let validCommands = take (i + 1) commands
      firstLight = True -- Simplified: always treat as first light
  in foldl (executeCommand firstLight) initialState validCommands

-- | Display the analysis state at step i as LaTeX
display :: Int -> String
display i = 
  let state = computeState i
      visibleEqs = filter eqVisible (stateResult state)
      renderedEqs = map renderEquation visibleEqs
      eqnArray = unlines renderedEqs
  in "\\[\\begin{eqnarray*}\n" ++ concatMap (++ " \\\\\n") renderedEqs ++ "\\end{eqnarray*}\\]\n"