{-# LANGUAGE DeriveFoldable, DeriveFunctor, TemplateHaskellQuotes, Safe #-}

module Language.Haskell.Exts.Extension.Mapping (
  -- * Specify the mapping items
    ExtensionMapping(NoMapping, ImpliedBy, Matches, ByDefaultEnabled)
  , mapItem
  -- * 'Extension' to 'KnownExtension'
  , mapToKnownExt, mapToKnownExts, mapToKnownExts'
  -- * 'KnownExtension' to 'Extension'
  , mapToExt, mapToExts, mapToExts'
  -- * Determine which extensions are enabled
  , knownExtsEnabled, knownExtsEnabled'
  , knownExtsEnabledWithErrors, knownExtsEnabledWithErrors'
  , knownExtsEnabledWithErrorsAndWarnings, knownExtsEnabledWithErrorsAndWarnings'
  ) where

import Data.List(intercalate)
import Data.Maybe(mapMaybe)

import Language.Haskell.TH(Q, extsEnabled, reportError, reportWarning)
import qualified Language.Haskell.TH.LanguageExtensions as T
import Language.Haskell.TH.LanguageExtensions(Extension)
import Language.Haskell.Exts.Extension(KnownExtension(..))

data ExtensionMapping a
  = NoMapping
  | ImpliedBy [a]
  | Matches a
  | ByDefaultEnabled
  deriving (Eq, Foldable, Functor, Ord, Read, Show)

instance Bounded (ExtensionMapping a) where
  minBound = NoMapping
  maxBound = ByDefaultEnabled

mapItem :: b -> ([a] -> b) -> (a -> b) -> b -> ExtensionMapping a -> b
mapItem a f g b = go
  where go NoMapping = a
        go (ImpliedBy xs) = f xs
        go (Matches x) = g x
        go ByDefaultEnabled = b

_dispatch :: (a -> ExtensionMapping b) -> [a] -> ([a], [(a, [b])], [b], [a])
_dispatch mp = foldr (f <*> mp) ([], [], [], [])
  where f x0 m ~(as, ys, xs, bs) = go m
          where go NoMapping = (x0 : as, ys, xs, bs)
                go (ImpliedBy y) = (as, (x0, y) : ys, xs, bs)
                go (Matches x) = (as, ys, x : xs, bs)
                go ByDefaultEnabled = (as, ys, xs, x0:bs)

_dispatch' :: (a -> ExtensionMapping b) -> [a] -> [b]
_dispatch' m = mapMaybe (mapItem Nothing (const Nothing) Just Nothing . m)

_sel42 :: Functor f => f (a, b, c, d) -> f c
_sel42 = fmap (\(_, _, c, _) -> c)

mapToKnownExt :: Extension -> ExtensionMapping KnownExtension
mapToKnownExt T.Cpp = Matches CPP
mapToKnownExt T.OverlappingInstances = Matches OverlappingInstances
mapToKnownExt T.UndecidableInstances = Matches UndecidableInstances
mapToKnownExt T.IncoherentInstances = Matches IncoherentInstances
-- mapToKnownExt T.UndecidableSuperClasses = Matches UndecidableSuperClasses (no generalizer)
mapToKnownExt T.MonomorphismRestriction = Matches MonomorphismRestriction
mapToKnownExt T.MonoPatBinds = Matches MonoPatBinds
mapToKnownExt T.MonoLocalBinds = Matches MonoLocalBinds
mapToKnownExt T.RelaxedPolyRec = Matches RelaxedPolyRec
mapToKnownExt T.ExtendedDefaultRules = Matches ExtendedDefaultRules
mapToKnownExt T.ForeignFunctionInterface = Matches ForeignFunctionInterface
mapToKnownExt T.UnliftedFFITypes = Matches UnliftedFFITypes
mapToKnownExt T.InterruptibleFFI = Matches InterruptibleFFI
mapToKnownExt T.CApiFFI = Matches CApiFFI
mapToKnownExt T.GHCForeignImportPrim = Matches GHCForeignImportPrim
mapToKnownExt T.JavaScriptFFI = Matches JavaScriptFFI
mapToKnownExt T.ParallelArrays = Matches ParallelArrays
mapToKnownExt T.Arrows = Matches Arrows
mapToKnownExt T.TemplateHaskell = Matches TemplateHaskell
mapToKnownExt T.TemplateHaskellQuotes = ImpliedBy [TemplateHaskell]
mapToKnownExt T.QuasiQuotes = Matches QuasiQuotes
mapToKnownExt T.ImplicitParams = Matches ImplicitParams
mapToKnownExt T.ImplicitPrelude = Matches ImplicitPrelude
mapToKnownExt T.ScopedTypeVariables = Matches ScopedTypeVariables
-- mapToKnownExt T.AllowAmbiguousTypes = Matches AllowAmbiguousTypes
mapToKnownExt T.UnboxedTuples = Matches UnboxedTuples
mapToKnownExt T.UnboxedSums = Matches UnboxedSums
-- mapToKnownExt T.UnliftedNewtypes = Matches UnliftedNewtypes
mapToKnownExt T.BangPatterns = Matches BangPatterns
mapToKnownExt T.TypeFamilies = Matches TypeFamilies
mapToKnownExt T.TypeFamilyDependencies = Matches TypeFamilyDependencies
mapToKnownExt T.TypeInType = Matches TypeInType
mapToKnownExt T.OverloadedStrings = Matches OverloadedStrings
-- mapToKnownExt T.OverloadedLists = Matches OverloadedLists
-- mapToKnownExt T.NumDecimals = Matches NumDecimals
mapToKnownExt T.DisambiguateRecordFields = Matches DisambiguateRecordFields
mapToKnownExt T.RecordWildCards = Matches RecordWildCards
mapToKnownExt T.RecordPuns = Matches RecordPuns
mapToKnownExt T.ViewPatterns = Matches ViewPatterns
mapToKnownExt T.GADTs = Matches GADTs
mapToKnownExt T.GADTSyntax = ImpliedBy [GADTs]
mapToKnownExt T.NPlusKPatterns = Matches NPlusKPatterns
mapToKnownExt T.DoAndIfThenElse = Matches DoAndIfThenElse
mapToKnownExt T.BlockArguments = Matches BlockArguments
mapToKnownExt T.RebindableSyntax = Matches RebindableSyntax
mapToKnownExt T.ConstraintKinds = Matches ConstraintKinds
mapToKnownExt T.PolyKinds = Matches PolyKinds
mapToKnownExt T.DataKinds = Matches DataKinds
mapToKnownExt T.InstanceSigs = Matches InstanceSigs
-- mapToKnownExt T.ApplicativeDo = Matches ApplicativeDo
mapToKnownExt T.StandaloneDeriving = Matches StandaloneDeriving
mapToKnownExt T.DeriveDataTypeable = Matches DeriveDataTypeable
mapToKnownExt T.AutoDeriveTypeable = ByDefaultEnabled
mapToKnownExt T.DeriveFunctor = Matches DeriveFunctor
mapToKnownExt T.DeriveTraversable = Matches DeriveTraversable
mapToKnownExt T.DeriveFoldable = Matches DeriveFoldable
mapToKnownExt T.DeriveGeneric = Matches DeriveGeneric
mapToKnownExt T.DefaultSignatures = Matches DefaultSignatures
mapToKnownExt T.DeriveAnyClass = Matches DeriveAnyClass
-- mapToKnownExt T.DeriveLift = Matches DeriveLift
mapToKnownExt T.DerivingStrategies = Matches DerivingStrategies
mapToKnownExt T.DerivingVia = Matches DerivingVia
mapToKnownExt T.TypeSynonymInstances = Matches TypeSynonymInstances
mapToKnownExt T.FlexibleContexts = Matches FlexibleContexts
mapToKnownExt T.FlexibleInstances = Matches FlexibleInstances
mapToKnownExt T.ConstrainedClassMethods = Matches ConstrainedClassMethods
mapToKnownExt T.MultiParamTypeClasses = Matches MultiParamTypeClasses
-- mapToKnownExt T.NullaryTypeClasses = Matches NullaryTypeClasses
mapToKnownExt T.FunctionalDependencies = Matches FunctionalDependencies
mapToKnownExt T.UnicodeSyntax = Matches UnicodeSyntax
mapToKnownExt T.ExistentialQuantification = Matches ExistentialQuantification
mapToKnownExt T.MagicHash = Matches MagicHash
mapToKnownExt T.EmptyDataDecls = Matches EmptyDataDecls
mapToKnownExt T.KindSignatures = Matches KindSignatures
mapToKnownExt T.RoleAnnotations = Matches RoleAnnotations
mapToKnownExt T.ParallelListComp = Matches ParallelListComp
mapToKnownExt T.TransformListComp = Matches TransformListComp
-- mapToKnownExt T.MonadComprehensions = Matches MonadComprehensions
mapToKnownExt T.GeneralizedNewtypeDeriving = Matches GeneralizedNewtypeDeriving
mapToKnownExt T.RecursiveDo = Matches RecursiveDo
mapToKnownExt T.PostfixOperators = Matches PostfixOperators
mapToKnownExt T.TupleSections = Matches TupleSections
mapToKnownExt T.PatternGuards = Matches PatternGuards
mapToKnownExt T.LiberalTypeSynonyms = Matches LiberalTypeSynonyms
mapToKnownExt T.RankNTypes = Matches RankNTypes
mapToKnownExt T.ImpredicativeTypes = Matches ImpredicativeTypes
mapToKnownExt T.TypeOperators = Matches TypeOperators
mapToKnownExt T.ExplicitNamespaces = Matches ExplicitNamespaces
mapToKnownExt T.PackageImports = Matches PackageImports
mapToKnownExt T.ExplicitForAll = Matches ExplicitForAll
-- mapToKnownExt T.AlternativeLayoutRule = Matches AlternativeLayoutRule  (not documented, unused)
-- mapToKnownExt T.AlternativeLayoutRuleTransitional = Matches AlternativeLayoutRuleTransitional  (not documented, unused)
mapToKnownExt T.DatatypeContexts = Matches DatatypeContexts
mapToKnownExt T.NondecreasingIndentation = Matches NondecreasingIndentation
-- mapToKnownExt T.RelaxedLayout = Matches RelaxedLayout
-- mapToKnownExt T.TraditionalRecordSyntax = Matches TraditionalRecordSyntax
mapToKnownExt T.LambdaCase = Matches LambdaCase
mapToKnownExt T.MultiWayIf = Matches MultiWayIf
mapToKnownExt T.BinaryLiterals = Matches BinaryLiterals
-- mapToKnownExt T.NegativeLiterals = Matches NegativeLiterals
-- mapToKnownExt T.HexFloatLiterals = Matches HexFloatLiterals
-- mapToKnownExt T.DuplicateRecordFields = Matches DuplicateRecordFields
mapToKnownExt T.OverloadedLabels = Matches OverloadedLabels
mapToKnownExt T.EmptyCase = Matches EmptyCase
mapToKnownExt T.PatternSynonyms = Matches PatternSynonyms
mapToKnownExt T.PartialTypeSignatures = Matches PartialTypeSignatures
mapToKnownExt T.NamedWildCards = Matches NamedWildCards
-- mapToKnownExt T.StaticPointers = Matches StaticPointers
mapToKnownExt T.TypeApplications = Matches TypeApplications
mapToKnownExt T.Strict = Matches Strict
mapToKnownExt T.StrictData = Matches StrictData
-- mapToKnownExt T.MonadFailDesugaring = Matches MonadFailDesugaring
-- mapToKnownExt T.EmptyDataDeriving = Matches EmptyDataDeriving
-- mapToKnownExt T.NumericUnderscores = Matches NumericUnderscores
mapToKnownExt T.QuantifiedConstraints = Matches QuantifiedConstraints
-- mapToKnownExt T.StarIsType = Matches StarIsType
-- mapToKnownExt T.ImportQualifiedPost = Matches ImportQualifiedPost
-- mapToKnownExt T.CUSKs = Matches CUSKs
-- mapToKnownExt T.StandaloneKindSignatures = Matches StandaloneKindSignatures
mapToKnownExt _ = NoMapping

mapToKnownExts :: [Extension] -> ([Extension], [(Extension, [KnownExtension])], [KnownExtension], [Extension])
mapToKnownExts = _dispatch mapToKnownExt

mapToKnownExts' :: [Extension] -> [KnownExtension]
mapToKnownExts' = _dispatch' mapToKnownExt

knownExtsEnabled :: Q [KnownExtension]
knownExtsEnabled = mapToKnownExts' <$> extsEnabled

knownExtsEnabled' :: Q ([Extension], [(Extension, [KnownExtension])], [KnownExtension], [Extension])
knownExtsEnabled' = mapToKnownExts <$> extsEnabled

_reportUnmappableExtension' :: String -> Extension -> Q ()
_reportUnmappableExtension' ss ext = reportError ("Can not find a mapping for the " ++ show ext ++ " as " ++ show ''KnownExtension ++ ss ++ ".")

_reportUnmappableExtension :: Extension -> Q ()
_reportUnmappableExtension = _reportUnmappableExtension' ""

_reportImpliableExtension :: Extension -> [KnownExtension] -> Q ()
_reportImpliableExtension = flip (_reportUnmappableExtension' . (". It can however be *implied* by the following extensions: " ++) . intercalate ", " . map show)

_reportEnabledExtension :: Extension -> Q ()
_reportEnabledExtension ext = reportWarning ("The " ++ show ext ++ " is enabled by default, and can not be mapped to a " ++ show ''KnownExtension ++ " item.")

knownExtsEnabledWithErrors' :: Q ([Extension], [(Extension, [KnownExtension])], [KnownExtension], [Extension])
knownExtsEnabledWithErrors' = do
  r@(we, im, _, _) <- knownExtsEnabled'
  mapM_ _reportUnmappableExtension we
  r <$ mapM_ (uncurry _reportImpliableExtension) im

knownExtsEnabledWithErrors :: Q [KnownExtension]
knownExtsEnabledWithErrors = _sel42 knownExtsEnabledWithErrors'

knownExtsEnabledWithErrorsAndWarnings' :: Q ([Extension], [(Extension, [KnownExtension])], [KnownExtension], [Extension])
knownExtsEnabledWithErrorsAndWarnings' = do
  r@(_, _, _, en) <- knownExtsEnabledWithErrors'
  r <$ mapM_ _reportEnabledExtension en

knownExtsEnabledWithErrorsAndWarnings :: Q [KnownExtension]
knownExtsEnabledWithErrorsAndWarnings = _sel42 knownExtsEnabledWithErrorsAndWarnings'

mapToExt :: KnownExtension -> ExtensionMapping Extension
mapToExt OverlappingInstances = Matches T.OverlappingInstances
mapToExt UndecidableInstances = Matches T.UndecidableInstances
mapToExt IncoherentInstances = Matches T.IncoherentInstances
mapToExt InstanceSigs = Matches T.InstanceSigs
-- mapToExt DoRec = Matches T.DoRec
mapToExt RecursiveDo = Matches T.RecursiveDo
mapToExt ParallelListComp = Matches T.ParallelListComp
mapToExt MultiParamTypeClasses = Matches T.MultiParamTypeClasses
mapToExt MonomorphismRestriction = Matches T.MonomorphismRestriction
mapToExt FunctionalDependencies = Matches T.FunctionalDependencies
mapToExt Rank2Types = ImpliedBy [T.RankNTypes]
mapToExt RankNTypes = Matches T.RankNTypes
-- mapToExt PolymorphicComponents = Matches T.PolymorphicComponents
mapToExt ExistentialQuantification = Matches T.ExistentialQuantification
mapToExt ScopedTypeVariables = Matches T.ScopedTypeVariables
-- mapToExt PatternSignatures = Matches T.PatternSignatures
mapToExt ImplicitParams = Matches T.ImplicitParams
mapToExt FlexibleContexts = Matches T.FlexibleContexts
mapToExt FlexibleInstances = Matches T.FlexibleInstances
mapToExt EmptyDataDecls = Matches T.EmptyDataDecls
mapToExt CPP = Matches T.Cpp
mapToExt KindSignatures = Matches T.KindSignatures
mapToExt BangPatterns = Matches T.BangPatterns
mapToExt TypeSynonymInstances = Matches T.TypeSynonymInstances
mapToExt TemplateHaskell = Matches T.TemplateHaskell
mapToExt ForeignFunctionInterface = Matches T.ForeignFunctionInterface
mapToExt Arrows = Matches T.Arrows
-- mapToExt Generics = Matches T.Generics
mapToExt ImplicitPrelude = Matches T.ImplicitPrelude
-- mapToExt NamedFieldPuns = Matches T.NamedFieldPuns
mapToExt PatternGuards = Matches T.PatternGuards
mapToExt GeneralizedNewtypeDeriving = Matches T.GeneralizedNewtypeDeriving
mapToExt DeriveAnyClass = Matches T.DeriveAnyClass
-- mapToExt ExtensibleRecords = Matches T.ExtensibleRecords
-- mapToExt RestrictedTypeSynonyms = Matches T.RestrictedTypeSynonyms
-- mapToExt HereDocuments = Matches T.HereDocuments
mapToExt MagicHash = Matches T.MagicHash
mapToExt BinaryLiterals = Matches T.BinaryLiterals
mapToExt TypeFamilies = Matches T.TypeFamilies
mapToExt StandaloneDeriving = Matches T.StandaloneDeriving
mapToExt UnicodeSyntax = Matches T.UnicodeSyntax
mapToExt UnliftedFFITypes = Matches T.UnliftedFFITypes
mapToExt LiberalTypeSynonyms = Matches T.LiberalTypeSynonyms
mapToExt TypeOperators = Matches T.TypeOperators
mapToExt ParallelArrays = Matches T.ParallelArrays
mapToExt RecordWildCards = Matches T.RecordWildCards
mapToExt RecordPuns = Matches T.RecordPuns
mapToExt DisambiguateRecordFields = Matches T.DisambiguateRecordFields
mapToExt OverloadedStrings = Matches T.OverloadedStrings
mapToExt GADTs = Matches T.GADTs
mapToExt MonoPatBinds = Matches T.MonoPatBinds
mapToExt RelaxedPolyRec = Matches T.RelaxedPolyRec
mapToExt ExtendedDefaultRules = Matches T.ExtendedDefaultRules
mapToExt UnboxedTuples = Matches T.UnboxedTuples
mapToExt DeriveDataTypeable = Matches T.DeriveDataTypeable
mapToExt ConstrainedClassMethods = Matches T.ConstrainedClassMethods
mapToExt PackageImports = Matches T.PackageImports
mapToExt LambdaCase = Matches T.LambdaCase
mapToExt EmptyCase = Matches T.EmptyCase
mapToExt ImpredicativeTypes = Matches T.ImpredicativeTypes
-- mapToExt NewQualifiedOperators = Matches T.NewQualifiedOperators
mapToExt PostfixOperators = Matches T.PostfixOperators
mapToExt QuasiQuotes = Matches T.QuasiQuotes
mapToExt TransformListComp = Matches T.TransformListComp
mapToExt ViewPatterns = Matches T.ViewPatterns
-- mapToExt XmlSyntax = Matches T.XmlSyntax
-- mapToExt RegularPatterns = Matches T.RegularPatterns
mapToExt TupleSections = Matches T.TupleSections
mapToExt GHCForeignImportPrim = Matches T.GHCForeignImportPrim
mapToExt NPlusKPatterns = Matches T.NPlusKPatterns
mapToExt DoAndIfThenElse = Matches T.DoAndIfThenElse
mapToExt RebindableSyntax = Matches T.RebindableSyntax
mapToExt ExplicitForAll = Matches T.ExplicitForAll
mapToExt DatatypeContexts = Matches T.DatatypeContexts
mapToExt MonoLocalBinds = Matches T.MonoLocalBinds
mapToExt DeriveFunctor = Matches T.DeriveFunctor
mapToExt DeriveGeneric = Matches T.DeriveGeneric
mapToExt DeriveTraversable = Matches T.DeriveTraversable
mapToExt DeriveFoldable = Matches T.DeriveFoldable
mapToExt NondecreasingIndentation = Matches T.NondecreasingIndentation
mapToExt InterruptibleFFI = Matches T.InterruptibleFFI
mapToExt CApiFFI = Matches T.CApiFFI
mapToExt JavaScriptFFI = Matches T.JavaScriptFFI
mapToExt ExplicitNamespaces = Matches T.ExplicitNamespaces
mapToExt DataKinds = Matches T.DataKinds
mapToExt PolyKinds = Matches T.PolyKinds
mapToExt MultiWayIf = Matches T.MultiWayIf
-- mapToExt SafeImports = Matches T.SafeImports
-- mapToExt Safe = Matches T.Safe
-- mapToExt Trustworthy = Matches T.Trustworthy
mapToExt DefaultSignatures = Matches T.DefaultSignatures
mapToExt ConstraintKinds = Matches T.ConstraintKinds
mapToExt RoleAnnotations = Matches T.RoleAnnotations
mapToExt PatternSynonyms = Matches T.PatternSynonyms
mapToExt PartialTypeSignatures = Matches T.PartialTypeSignatures
mapToExt NamedWildCards = Matches T.NamedWildCards
mapToExt TypeApplications = Matches T.TypeApplications
mapToExt TypeFamilyDependencies = Matches T.TypeFamilyDependencies
mapToExt OverloadedLabels = Matches T.OverloadedLabels
mapToExt DerivingStrategies = Matches T.DerivingStrategies
mapToExt UnboxedSums = Matches T.UnboxedSums
mapToExt TypeInType = Matches T.TypeInType
mapToExt Strict = Matches T.Strict
mapToExt StrictData = Matches T.StrictData
mapToExt DerivingVia = Matches T.DerivingVia
mapToExt QuantifiedConstraints = Matches T.QuantifiedConstraints
mapToExt BlockArguments = Matches T.BlockArguments
mapToExt _ = NoMapping

mapToExts :: [KnownExtension] -> ([KnownExtension], [(KnownExtension, [Extension])], [Extension], [KnownExtension])
mapToExts = _dispatch mapToExt

mapToExts' :: [KnownExtension] -> [Extension]
mapToExts' = _dispatch' mapToExt
