
-- | Univariate polynomials

module Class.Poly where

--------------------------------------------------------------------------------

import Data.Kind

import Class.Field

--------------------------------------------------------------------------------
-- * Dense univariate polynomials over (finite) fields

class (Ring p, Field (Coeff p)) => Univariate p where
  -- | the type of coefficients
  type Coeff p :: Type
  -- | Degree
  degree :: p -> Int
  -- | Size (can be larger than degree+1 if the top coefficients are zeor)
  polySize  :: p -> Int
  -- | The k-th coefficient
  kthCoeff :: Int -> p -> Coeff p
  -- | Evaluation
  evalAt :: Coeff p -> p -> Coeff p
  -- | Scaling
  scale :: Coeff p -> p -> p
  -- | Create a polynomial from coefficiens
  mkPoly :: [Coeff p] -> p
  -- | Coefficients of the polynomial as a list
  coeffs :: p -> [Coeff p]
  -- | Coefficients as an Array
  coeffsArr :: p -> Array Int (Coeff p)
{-
  -- | Polynomial long division
  polyLongDiv :: p -> p -> (p,p)
  -- | Polynomial quotient
  polyQuot :: p -> p -> p
  -- | Polynomial remainder
  polyRem :: p -> p -> p
  -- | Divide by the coset vanishing polynomial @(x^n - eta)@
  divByVanishing :: p -> (Int, Coeff p) -> (p,p)
  -- | Quotient by the coset vanishing polynomial @(x^n - eta)@
  quotByVanishing :: p -> (Int, Coeff p) -> Maybe p
-}

--------------------------------------------------------------------------------
-- * Some generic functions

-- | Checks whether the input is the constant one polynomial?
polyIsOne :: Univariate p => p -> Bool
polyIsOne p = case mbConst p of
  Nothing -> False
  Just x  -> ZK.Algebra.Class.Field.isOne x

-- | The constant term of a polynomial
constTermOf :: Univariate p => p -> Coeff p
constTermOf = kthCoeff 0

-- | Is this a constant polynomial? 
--
-- TODO: this is not efficient for high-degree polynomials, where we could exit early...
mbConst :: Univariate p => p -> Maybe (Coeff p)
mbConst p = if degree p <= 0 then Just (constTermOf p) else Nothing

-- | Create a constant polynomial
constPoly :: Univariate p => Coeff p -> p
constPoly y = mkPoly [y]

-- | The polynomial @p(x)=x@
idPoly :: Univariate p => p
idPoly = mkPoly [0,1]

-- | Create a linear polynomial.
-- 
-- > linearPoly a b == a*x + b@
--
linearPoly :: Univariate p => Coeff p -> Coeff p -> p
linearPoly a b = mkPoly [b,a]

showPoly :: Univariate p => p -> String
showPoly = showPoly' True

showPoly' :: Univariate p => Bool -> p -> String
showPoly' newlines_flag poly =
  case newlines_flag of
    False -> intercalate " +"   terms
    True  -> intercalate " +\n" terms
  where
    pairs  = filter (\kx -> snd kx /= 0) 
           $ zip [0..] (coeffs poly)
    terms  = case pairs of 
               [] -> [" 0"]
               _  -> map f pairs
    f (0,x) = ' ' : show x
    f (1,x) = ' ' : show x ++ " * x"
    f (k,x) = ' ' : show x ++ " * x^" ++ show k

--------------------------------------------------------------------------------