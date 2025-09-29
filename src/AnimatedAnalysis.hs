module AnimatedAnalysis (display) where

import Data.Text qualified as T
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

-- | Represents an equation in the analysis with its state and formatting
data Equation = Equation
  { eqBinding :: T.Text
  , eqValue :: T.Text
  , eqVisible :: Bool
  , eqActionable :: Bool
  , eqNewlyCreated :: Bool
  , eqNewlyUpdated :: Bool
  } deriving (Show, Eq)

-- | Represents a binding with its program point and possible values
data Binding = Binding
  { bindPoint :: T.Text
  , bindValues :: [T.Text]
  , bindAffects :: [T.Text]
  } deriving (Show)

-- | Analysis state containing equations and metadata
data State = State
  { stateData :: M.Map T.Text Binding
  , stateResult :: [Equation]
  , stateResIndexes :: M.Map T.Text Int
  , stateContext :: Maybe T.Text
  , stateLastName :: Maybe T.Text
  , stateCreatedAt :: M.Map Int Int  -- equation index -> step created
  , stateUpdatedAt :: M.Map Int Int  -- equation index -> step last updated
  } deriving (Show)

-- | Commands that can be executed to modify the analysis state
data Command
  = Create T.Text
  | Act T.Text Command
  | Act2 T.Text Command Command
  | Evaluate (Maybe T.Text)
  | Hiding [T.Text] Command
  | Depends T.Text
  deriving (Show)

-- | Create a new equation with the given binding
mkEquation :: T.Text -> Bool -> Equation
mkEquation binding newlyCreated = Equation
  { eqBinding = binding
  , eqValue = "\\bot"
  , eqVisible = True
  , eqActionable = True
  , eqNewlyCreated = newlyCreated
  , eqNewlyUpdated = False
  }

-- | Update an equation's value and newly_updated flag
updateEquation :: Equation -> T.Text -> Bool -> Equation
updateEquation eq newValue newlyUpdated = eq
  { eqValue = newValue
  , eqActionable = False
  , eqNewlyUpdated = newlyUpdated
  }

-- | Render an equation as LaTeX
renderEquation :: Equation -> T.Text
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
initialData :: M.Map T.Text Binding
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
  , stateCreatedAt = M.empty
  , stateUpdatedAt = M.empty
  }

-- | Execute a single command on the state
executeCommand :: Int -> Bool -> State -> Command -> State
executeCommand step firstLight state cmd = case cmd of
  Create name -> createBinding step firstLight name state
  Act name subCmd -> actOnBinding step firstLight name subCmd state
  Act2 name cmd1 cmd2 -> actOnBinding step firstLight name cmd2 (actOnBinding step firstLight name cmd1 state)
  Evaluate mName -> evaluateBinding step firstLight (fromMaybe (fromMaybe T.empty (stateLastName state)) mName) state
  Hiding names subCmd -> hideBindings names (executeCommand step firstLight state subCmd)
  Depends name -> dependsOnContext name state

-- | Create a new binding equation
createBinding :: Int -> Bool -> T.Text -> State -> State
createBinding step firstLight name state =
  let binding = M.findWithDefault (Binding T.empty [] []) name (stateData state)
      newEq = mkEquation (bindPoint binding) firstLight
      newIndex = length (stateResult state)
      newState = state
        { stateResult = stateResult state ++ [newEq]
        , stateResIndexes = M.insert name newIndex (stateResIndexes state)
        , stateLastName = Just name
        , stateCreatedAt = M.insert newIndex step (stateCreatedAt state)
        }
  in case stateContext state of
       Just ctx -> addDependency name ctx newState
       Nothing -> newState

-- | Act on a binding (make it non-actionable and execute subcommand)
actOnBinding :: Int -> Bool -> T.Text -> Command -> State -> State
actOnBinding step firstLight name subCmd state =
  let maybeIdx = M.lookup name (stateResIndexes state)
  in case maybeIdx of
       Just idx ->
         let oldEq = stateResult state !! idx
             newEq = oldEq { eqActionable = False }
             newResult = take idx (stateResult state) ++ [newEq] ++ drop (idx + 1) (stateResult state)
             newState = state { stateResult = newResult, stateContext = Just name }
             finalState = executeCommand step firstLight newState subCmd
         in finalState { stateContext = Nothing }
       Nothing -> state

-- | Evaluate a binding (update its value)
evaluateBinding :: Int -> Bool -> T.Text -> State -> State
evaluateBinding step firstLight name state =
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
             -- Track when this equation was updated
             newUpdatedAt = if oldValue /= newValue
                           then M.insert idx step (stateUpdatedAt state)
                           else stateUpdatedAt state
         in state { stateResult = newResult2, stateData = newData, stateUpdatedAt = newUpdatedAt }
       _ -> state

-- | Make affected bindings actionable
makeAffectedActionable :: [T.Text] -> State -> [Equation] -> [Equation]
makeAffectedActionable affects state result =
  foldr (\name acc -> case M.lookup name (stateResIndexes state) of
                        Just idx | idx < length acc ->
                          let eq = acc !! idx
                              newEq = eq { eqActionable = True }
                          in take idx acc ++ [newEq] ++ drop (idx + 1) acc
                        _ -> acc) result affects

-- | Hide specified bindings
hideBindings :: [T.Text] -> State -> State
hideBindings names state =
  let newResult = map (\eq ->
        if any (\name -> case M.lookup name (stateResIndexes state) of
                           Just idx -> stateResult state !! idx == eq
                           Nothing -> False) names
        then eq { eqVisible = False }
        else eq) (stateResult state)
  in state { stateResult = newResult }

-- | Add dependency relationship - adds context to new binding's affects list
addDependency :: T.Text -> T.Text -> State -> State
addDependency name ctxName state =
  case M.lookup name (stateData state) of
    Just binding ->
      let updatedBinding = binding { bindAffects = bindAffects binding ++ [ctxName] }
          newData = M.insert name updatedBinding (stateData state)
      in state { stateData = newData }
    Nothing -> state

-- | Add dependency (used by Depends command)
dependsOnContext :: T.Text -> State -> State
dependsOnContext name state = case stateContext state of
  Just ctx -> addDependency name ctx state
  Nothing -> state

-- | Sequence of display calls as they appear in the Ruby template
displaySequence :: [Int]
displaySequence = [0,1,2,3,4,5,6,7,8,16,19,20,21,23,26,27,28,38,39,40,41,42,43,44]

-- | Get the previous display step for simulating @@last_displayed
getPreviousDisplayStep :: Int -> Int
getPreviousDisplayStep i =
  case takeWhile (< i) displaySequence of
    [] -> -1  -- Initial value of @@last_displayed
    prev -> last prev

-- | Execute commands up to step i and return final state
computeState :: Int -> Int -> State
computeState targetStep i =
  let validCommands = take (i + 1) commands
      lastDisplayed = getPreviousDisplayStep targetStep
      -- Steps after lastDisplayed get first_light = true (simulating Ruby's @@last_displayed logic)
      stepHasFirstLight step = step > lastDisplayed
  in foldl (\state (step, cmd) -> executeCommand step (stepHasFirstLight step) state cmd)
           initialState
           (zip [0..] validCommands)

-- | Display the analysis state at step i as LaTeX
display :: Int -> T.Text
display i =
  let finalState = computeState i i
      visibleEqs = filter eqVisible (stateResult finalState)
      renderedEqs = map renderEquation visibleEqs
      eqnLines = case renderedEqs of
                   [] -> []
                   [single] -> [single]
                   multiple -> map (<> " \\\\") (init multiple) ++ [last multiple]
  in "\\[\\begin{aligned}\n" <> T.concat (map (<> "\n") eqnLines) <> "\\end{aligned}\\]\n"
