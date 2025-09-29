module AnimatedAnalysis (display) where

import Data.Text qualified as T
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))

-- | Newtype for binding names used as lookup keys
newtype BindingName = BindingName T.Text
  deriving (Show, Eq, Ord)

instance IsString BindingName where
  fromString = BindingName . T.pack

-- | Represents an equation in the analysis with its state and formatting
data Equation = Equation
  { eqBinding :: T.Text
  , eqValue :: T.Text
  , eqVisible :: Bool
  , eqActionable :: Bool
  , eqCreatedAt :: Int
  , eqLastUpdatedAt :: Maybe Int
  } deriving (Show, Eq)

-- | Represents a binding with its program point and possible values
data Binding = Binding
  { bindPoint :: T.Text
  , bindValues :: [T.Text]
  , bindAffects :: [BindingName]
  } deriving (Show)

-- | Analysis state containing equations and metadata
data State = State
  { stateData :: M.Map BindingName Binding
  , stateResult :: [Equation]
  , stateResIndexes :: M.Map BindingName Int
  , stateContext :: Maybe BindingName
  , stateLastName :: Maybe BindingName
  } deriving (Show)

-- | Commands that can be executed to modify the analysis state
data Command
  = Create BindingName
  | Act BindingName Command
  | Act2 BindingName Command Command
  | Evaluate (Maybe BindingName)
  | Hiding [BindingName] Command
  | Depends BindingName
  deriving (Show)

-- | Create a new equation with the given binding
mkEquation :: T.Text -> Int -> Equation
mkEquation binding createdAt = Equation
  { eqBinding = binding
  , eqValue = "\\bot"
  , eqVisible = True
  , eqActionable = True
  , eqCreatedAt = createdAt
  , eqLastUpdatedAt = Nothing
  }

-- | Update an equation's value and last updated timestamp
updateEquation :: Equation -> T.Text -> Int -> Equation
updateEquation eq newValue updatedAt = eq
  { eqValue = newValue
  , eqActionable = False
  , eqLastUpdatedAt = Just updatedAt
  }

-- | Render an equation as LaTeX
renderEquation :: Int -> Equation -> T.Text
renderEquation lastDisplayed eq = bindingR <> " & " <> mapstoR <> " & " <> valueR
  where
    newlyCreated = eqCreatedAt eq > lastDisplayed
    newlyUpdated = case eqLastUpdatedAt eq of
                     Just updatedAt -> updatedAt > lastDisplayed
                     Nothing -> False

    bindingR
      | eqActionable eq = "\\bbox[color:orange]{" <> eqBinding eq <> "}"
      | newlyCreated = "\\bbox[color:green]{" <> eqBinding eq <> "}"
      | otherwise = eqBinding eq

    mapstoR
      | newlyCreated = "\\bbox[color:green]{\\mapsto}"
      | otherwise = "\\mapsto"

    valueR
      | newlyCreated || newlyUpdated = "\\bbox[color:green]{" <> eqValue eq <> "}"
      | otherwise = eqValue eq

-- | Initial data bindings (from Ruby @data hash)
initialData :: M.Map BindingName Binding
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
executeCommand :: Int -> State -> Command -> State
executeCommand step state cmd = case cmd of
  Create name -> createBinding step name state
  Act name subCmd -> actOnBinding step name subCmd state
  Act2 name cmd1 cmd2 -> actOnBinding step name cmd2 (actOnBinding step name cmd1 state)
  Evaluate mName -> evaluateBinding step (fromMaybe (fromMaybe "" (stateLastName state)) mName) state
  Hiding names subCmd -> hideBindings names (executeCommand step state subCmd)
  Depends name -> dependsOnContext name state

-- | Create a new binding equation
createBinding :: Int -> BindingName -> State -> State
createBinding step name state =
  let binding = M.findWithDefault (Binding T.empty [] []) name (stateData state)
      newEq = mkEquation (bindPoint binding) step
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
actOnBinding :: Int -> BindingName -> Command -> State -> State
actOnBinding step name subCmd state =
  let maybeIdx = M.lookup name (stateResIndexes state)
  in case maybeIdx of
       Just idx ->
         let oldEq = stateResult state !! idx
             newEq = oldEq { eqActionable = False }
             newResult = take idx (stateResult state) ++ [newEq] ++ drop (idx + 1) (stateResult state)
             newState = state { stateResult = newResult, stateContext = Just name }
             finalState = executeCommand step newState subCmd
         in finalState { stateContext = Nothing }
       Nothing -> state

-- | Evaluate a binding (update its value)
evaluateBinding :: Int -> BindingName -> State -> State
evaluateBinding step name state =
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
                     then updateEquation oldEq newValue step
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
makeAffectedActionable :: [BindingName] -> State -> [Equation] -> [Equation]
makeAffectedActionable affects state result =
  foldr (\name acc -> case M.lookup name (stateResIndexes state) of
                        Just idx | idx < length acc ->
                          let eq = acc !! idx
                              newEq = eq { eqActionable = True }
                          in take idx acc ++ [newEq] ++ drop (idx + 1) acc
                        _ -> acc) result affects

-- | Hide specified bindings
hideBindings :: [BindingName] -> State -> State
hideBindings names state =
  let newResult = map (\eq ->
        if any (\name -> case M.lookup name (stateResIndexes state) of
                           Just idx -> stateResult state !! idx == eq
                           Nothing -> False) names
        then eq { eqVisible = False }
        else eq) (stateResult state)
  in state { stateResult = newResult }

-- | Add dependency relationship - adds context to new binding's affects list
addDependency :: BindingName -> BindingName -> State -> State
addDependency name ctxName state =
  case M.lookup name (stateData state) of
    Just binding ->
      let updatedBinding = binding { bindAffects = bindAffects binding ++ [ctxName] }
          newData = M.insert name updatedBinding (stateData state)
      in state { stateData = newData }
    Nothing -> state

-- | Add dependency (used by Depends command)
dependsOnContext :: BindingName -> State -> State
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
computeState :: Int -> State
computeState targetStep =
  let validCommands = take (targetStep + 1) commands
  in foldl (\state (step, cmd) -> executeCommand step state cmd)
           initialState
           (zip [0..] validCommands)

-- | Display the analysis state at step i as LaTeX
display :: Int -> T.Text
display i =
  let finalState = computeState i
      lastDisplayed = getPreviousDisplayStep i
      visibleEqs = filter eqVisible (stateResult finalState)
      renderedEqs = map (renderEquation lastDisplayed) visibleEqs
      eqnLines = case renderedEqs of
                   [] -> []
                   [single] -> [single]
                   multiple -> map (<> " \\\\") (init multiple) ++ [last multiple]
  in "\\[\\begin{aligned}\n" <> T.concat (map (<> "\n") eqnLines) <> "\\end{aligned}\\]\n"
