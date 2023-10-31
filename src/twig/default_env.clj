(ns twig.default-env
  (:require [twig.values :as values]
            [twig.environments :as env]
            [twig.primitives.base :as primitives]
            [twig.primitives.apply :as primitive-apply]
            [twig.primitives.lambda :as primitive-lambda]
            [twig.primitives.logic :as primitive-logic]
            [twig.primitives.let :as primitive-let]
            [twig.primitives.sequences :as primitive-sequences]
            [twig.standard-library :as standard-library]))

(def default
  {primitives/default-syntax (env/special primitive-apply/apply-syntax)
   "and" (env/special primitive-logic/and-syntax)
   "or" (env/special primitive-logic/or-syntax)
   "if" (env/special primitive-logic/if-syntax)
   "fn" (env/special primitive-lambda/lambda-syntax)
   "let" (env/special primitive-let/let-syntax)
   "letrec" (env/special primitive-let/letrec-syntax)

   "link" (env/special primitive-sequences/link-syntax)

   "false" (env/literal values/false-atom)
   "true" (env/literal values/true-atom)

   "+" (env/bind standard-library/add-proc)
   "-" (env/bind standard-library/negative)})
