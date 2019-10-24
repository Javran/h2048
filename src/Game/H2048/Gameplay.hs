module Game.H2048.Gameplay
  ( Gameplay
  , _gpRule
  , _gpScore
  , _gpBoard
  , _gpGen
  , randomOp
  , mkGameplay
  , spawnNewCell
  , GameBoard
  , CellTier
  , Cell
  , _cTier
  , Dir(..)
  , Distrib
  , GameRule(..)
  , newGame
  , stepGame
  , standardGameRule
  , hasWon
  , isAlive
  , cellToInt
  , intToCell
  , computeDistrib
  ) where

import Control.Monad.RWS.Strict
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.H2048.Core hiding (isAlive)
import qualified Game.H2048.Core as Core

{-
  Just a quick note that if we were to implement Gameplay
  in terms of monad transformer, MonadTransControl might be
  useful to allow passing IO actions to brick while keeping
  the stack of transformers.
 -}

{-|
  A 'Gameplay' is an obscure data type to keep track of information necessary
  for a single game play. Its fields can be accessed through functions
  with @_gp@ prefix.
 -}
data Gameplay
  = Gameplay
  {
    -- | Encodes rule of this game. This field must not change after creation.
    _gpRule :: GameRule
    -- | Total score currently collected.
  , _gpScore :: Int
    {-|
      The Game board. If this field is an empty map,
      that means the game is not yet started.
     -}
  , _gpBoard :: GameBoard
    -- | Random generator.
  , _gpGen :: TFGen
  }

{-|
  Lift a function that mutates a 'TFGen' to produce some results to
  work on 'Gameplay'.
 -}
randomOp :: (TFGen -> (a, TFGen)) -> Gameplay -> (a, Gameplay)
randomOp op gp = (v, gp { _gpGen = g' })
  where
    g = _gpGen gp
    (v, g') = op g

{-|
  Create a 'Gameplay'. Note that the return value must be passed to 'newGame'
  before it can accept any game moves.

  The purpose of this two-step approach (i.e. 'mkGameplay' then 'newGame') is
  to separate data type creation from the effect of mutating random generator,
  which is required at the beginning of a game.
 -}
mkGameplay :: TFGen -> GameRule -> Gameplay
mkGameplay g r =
  Gameplay
    r
    0
    M.empty -- default board is empty - no move is allowed on it.
    g

{-|
  @spawnNewCell gameplay emptyCells@ picks an empty cell from @emptyCells@,
  and assign it with a cell value. The operation will fail if and only if
  @emptyCells@ is empty.

  Upon successful return, the value wrapped in @Just@ is
  @(sepResult, gameplay')@ where @sepResult@ indicates coordinate and cell value
  chosen, and remaining part of @emptyCells@.

  The reason for explicitly passing 'emptyCells' on this operation
  is to make it easier to pick multiple cells while not touching most parts of 'Gameplay'.
  In fact you can expect this operation to only mutate the 'TFGen' inside 'Gameplay'.
 -}
spawnNewCell :: Gameplay -> S.Set Coord -> Maybe (((Coord, Cell), S.Set Coord), Gameplay)
spawnNewCell gp emptyCells = do
  False <- pure $ S.null emptyCells
  let -- step 1: pick an empty cell.
      lowHigh = (0, S.size emptyCells - 1)
      (i, gp') = randomOp (randomR lowHigh) gp
      v = S.toAscList emptyCells !! i
      -- step 2: pick a tier.
      distrib = _grNewCellDistrib . _gpRule $ gp'
      (tier, gp'') = randomOp (randomPick distrib) gp'
  pure (((v, Cell tier), S.delete v emptyCells), gp'')

{-|
  Initialize a 'Gameplay' so that it\'s ready to play.

  This function should only fail when its 'GameRule' dictates too many
  initial cells for the whole board to contain.
 -}
newGame :: Gameplay -> Gameplay
newGame gp =
    fix (\loop curGp spawnTodo emptyCells ->
            if spawnTodo <= 0
              then curGp
              else case spawnNewCell curGp emptyCells of
                Nothing ->
                  error "Failed to create new game, no more space for empty cells."
                Just (((coord, cell), emptyCells'), curGp') ->
                  let curGp'' = curGp' { _gpBoard = M.insert coord cell (_gpBoard curGp') }
                  in loop curGp'' (spawnTodo - 1) emptyCells'
        )
      (gp { _gpScore = 0 })
      initSpawn
      (S.fromDistinctAscList coords)
  where
    rule = _gpRule gp
    coords = allCoords rule
    initSpawn = _grInitSpawn rule

{-|
  @stepGame d gp@ tries to apply move @d@ on current state of the game @gp@, returns:

  * @Nothing@ if this move is invalid (failed to apply the move).
  * @Just moves@ if this move is valid, also returns all possible moves
    after the board is fully updated (meaning new cell has been spawned).
 -}
stepGame :: Dir -> Gameplay -> Maybe Gameplay
stepGame dir gp = do
  let rule = _gpRule gp
      coords = allCoords rule
      bd = _gpBoard gp
  {-
    if a move can be applied successfully, that means
    there must be empty cell on the board,
    in other words, spawnNewCell should not fail.
   -}
  (bd', award) <- applyMove rule dir bd
  let emptyCoords = S.filter (`M.notMember` bd') (S.fromDistinctAscList coords)
  (((coordNew, cellNew), _), gp') <- spawnNewCell gp emptyCoords
  pure gp'
    { _gpBoard = M.insert coordNew cellNew bd'
      {-
        This assumes that spawnNewCell does not change _gpScore,
        otherwise whatever update will be overwritten by following one.
       -}
    , _gpScore = _gpScore gp + award
    }

{-|
  A 'Gameplay' is considered alive if and only if there are still possible moves.
 -}
isAlive :: Gameplay -> Bool
isAlive = Core.isAlive <$> _gpRule <*> _gpBoard

{-|
  Whether the 'Gameplay' should be considered already won.
  Queries 'GameRule' embeded in 'Gameplay'.
 -}
hasWon :: Gameplay -> Bool
hasWon = (_grHasWon . _gpRule) <*> _gpBoard
