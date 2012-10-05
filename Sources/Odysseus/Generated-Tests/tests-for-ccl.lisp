#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 314159 42 1776 271828 6021023 1066))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . -1)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1245300010 577420908 1195473439 663422594 1326123026 568306390))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 401)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1152031786 369385656 1575173852 1381901982 219671064 496744375))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 803)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (no-operation) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2032109905 1153314205 1861581683 305022167 978293029 77023413))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 1205)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 518037624 886207658 1439030441 575621695 181072288 613886626))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 1985)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1832159205 726729406 1273157711 1859012553 954694965 819069485))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 2765)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1818458101 544191117 1639736918 978746089 550649713 757528028))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 3170)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 494354759 528630464 691435720 588934556 159862478 142257681))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 3572)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 791278515 1456505899 1592415206 1094289561 1210819851 442833095))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 3974)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 683282315 305260794 550631841 389896581 466224318 530851217))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 4381)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep annabelle)
       (do (eat annabelle) (do (celebrate annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-04)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2100724786 259139861 1275272877 1114338097 455126763 1905187388))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 4788)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (no-operation)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-05)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1595698840 2029785597 1491574198 1857878530 866176154 804217272))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 5195)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 219883255 1033260386 1348598178 270326902 1479342205 1815165838))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 5600)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1403206597 398524517 349673415 414730351 1240640502 2028450463))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 6384)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1511266242 280519000 1171530167 1355170319 1480661966 2075983509))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 7168)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-07)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1408360142 1348888885 1586046681 289743093 300287066 133888869))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 7952)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-08)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1360579387 1083515803 1300119373 347563201 688619820 419444740))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 8737)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (sleep annabelle) (do (work annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1305979108 1180749878 256726753 872060122 1842185246 2034109980))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 9143)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 840738127 1715433226 1887732698 1028863367 91494915 336530543))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 9551)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1697395399 444170286 1345963020 534589701 100974892 2063993110))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 10337)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 247241036 1577242354 1687653835 1682205497 905964940 1657213377))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 11122)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.3)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 211530737 728924270 2064764194 1753101049 555658287 1286312172))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 11906)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.4)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 893526824 85226344 903092430 1121332392 1375561755 251173569))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 12311)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.5)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2087321734 1446618403 1702290573 913884485 1248792113 2137633064))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 12716)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (work laith) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.6)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1083757885 1235497254 1321975919 1937485802 1385881535 1234476435))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 13124)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1107823760 389182262 1770582801 1737352836 2000849357 1735515809))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 13904)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 821859183 752522990 1976000152 1195964071 2641301 1805282799))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 14312)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 762596194 1148007502 782050999 2032507902 674221084 854389629))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 14720)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do (eat #:?sv0.person) s0) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 430370603 1438718040 586626670 1190129657 2145447028 1218535305))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 15130)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (eat annabelle) (do (eat annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 432707858 1868832766 289177540 2120415254 837248405 998709772))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 15539)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (sleep #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (work #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (sleep #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (work #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (sleep #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (work #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (sleep #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (work #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (eat #:?sv0.person)
                                                (do
                                                 (sleep
                                                  #:?sv0.person)
                                                 (do
                                                  (eat #:?sv0.person)
                                                  (do
                                                   (work
                                                    #:?sv0.person)
                                                   (do
                                                    (eat
                                                     #:?sv0.person)
                                                    (do
                                                     (eat
                                                      #:?sv0.person)
                                                     (do
                                                      (sleep
                                                       #:?sv0.person)
                                                      (do
                                                       (eat
                                                        #:?sv0.person)
                                                       (do
                                                        (work
                                                         #:?sv0.person)
                                                        (do
                                                         (eat
                                                          #:?sv0.person)
                                                         (do
                                                          (eat
                                                           #:?sv0.person)
                                                          (do
                                                           (sleep
                                                            #:?sv0.person)
                                                           (do
                                                            (eat
                                                             #:?sv0.person)
                                                            (do
                                                             (work
                                                              #:?sv0.person)
                                                             (do
                                                              (eat
                                                               #:?sv0.person)
                                                              (do
                                                               (eat
                                                                #:?sv0.person)
                                                               (do
                                                                (sleep
                                                                 #:?sv0.person)
                                                                (do
                                                                 (eat
                                                                  #:?sv0.person)
                                                                 (do
                                                                  (work
                                                                   #:?sv0.person)
                                                                  (do
                                                                   (eat
                                                                    #:?sv0.person)
                                                                   (do
                                                                    (eat
                                                                     #:?sv0.person)
                                                                    (do
                                                                     (sleep
                                                                      #:?sv0.person)
                                                                     (do
                                                                      (eat
                                                                       #:?sv0.person)
                                                                      (do
                                                                       (work
                                                                        #:?sv0.person)
                                                                       (do
                                                                        (eat
                                                                         #:?sv0.person)
                                                                        (do
                                                                         (eat
                                                                          #:?sv0.person)
                                                                         (do
                                                                          (sleep
                                                                           #:?sv0.person)
                                                                          (do
                                                                           (eat
                                                                            #:?sv0.person)
                                                                           (do
                                                                            (work
                                                                             #:?sv0.person)
                                                                            (do
                                                                             (eat
                                                                              #:?sv0.person)
                                                                             (do
                                                                              (eat
                                                                               #:?sv0.person)
                                                                              (do
                                                                               (sleep
                                                                                #:?sv0.person)
                                                                               (do
                                                                                (eat
                                                                                 #:?sv0.person)
                                                                                (do
                                                                                 (work
                                                                                  #:?sv0.person)
                                                                                 (do
                                                                                  (eat
                                                                                   #:?sv0.person)
                                                                                  s0 )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09g)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 735562803 2147366682 1042801594 2045521751 478083034 922528108))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 16004)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle)
            (do
             (eat annabelle)
             (do (eat annabelle)
                 (do
                  (eat annabelle)
                  (do (eat annabelle)
                      (do
                       (eat annabelle)
                       (do (eat annabelle)
                           (do
                            (eat annabelle)
                            (do (eat annabelle)
                                (do
                                 (eat annabelle)
                                 (do
                                  (eat annabelle)
                                  (do
                                   (eat annabelle)
                                   (do
                                    (eat annabelle)
                                    (do
                                     (eat annabelle)
                                     (do
                                      (eat annabelle)
                                      (do
                                       (eat annabelle)
                                       (do
                                        (eat annabelle)
                                        (do
                                         (eat annabelle)
                                         (do
                                          (eat annabelle)
                                          (do
                                           (eat annabelle)
                                           s0) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09h)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1578194658 295408408 562658452 583720294 308701982 2034907628))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 16430)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (eat #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (eat #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (eat #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (eat #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (eat #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (eat #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (eat #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (eat #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (eat #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (eat #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (sleep #:?sv0.person)
                                                s0 )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09i)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 979678010 101684861 1119859729 961319859 773570221 1002107338))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 16861)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09j)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1008227503 2004857068 2047992527 269989285 69734722 928297875))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 17676)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle)
            (do (eat annabelle) (do (eat annabelle) s0 )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2096228109 741755117 1342977131 1961771922 174618839 555451145))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 18087)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (work matthias)
       (do (celebrate matthias) (do (sleep matthias) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 864504621 226390932 580594863 1586331691 2032823963 316181051))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 18875)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2147162602 687009520 589958299 354091266 1494916052 1667030000))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 21180)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 255117855 1559927326 1479235507 646131343 1465284824 111418756))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 21968)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do (work laith) (do (celebrate laith) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-11)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2066718954 1212752570 1969539513 376478277 429155701 357747292))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 23511)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person) (do (sleep #:?sv0.person) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1540437811 1630423096 468403112 2121346176 700818658 370909650))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 23918)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 318145803 344291303 296541001 1896487939 2129181624 1523574949))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 24325)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 943785832 809193669 326521636 1951058985 1745625941 2016193903))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 24731)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1077324898 707692658 1581035309 1135454865 638526768 1426271549))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 25137)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 135377858 871360016 1728767488 1010085048 2051811210 31145062))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 25546)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1832281128 864746259 23986801 512776868 1708691936 438635472))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 26331)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 505671503 2107037079 1345324137 1846439384 1881658519 2066940352))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 26738)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 109633498 946146336 1510336813 514864783 828777427 707002668))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 27145)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep matthias) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 407454796 875567704 115100633 483559735 205921623 240036872))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 27551)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep #:?p27576.person) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1628015172 2004401547 1944542105 1854897223 209656529 1932480001))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 27954)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-15)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 602276040 1996969939 1158435930 749592840 2143447207 1168866899))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 28735)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 6950424 375924822 2099722923 2031229256 1600540221 709095897))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 30656)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2047862482 733934744 243594508 1078804069 307593908 871252656))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 34094)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 824331153 685371258 321526335 593115 2025512736 404102973))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 39052)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1400642844 1919965001 1276308801 1037525553 450753793 1287506670))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 39454)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 216522467 1715163393 1062321235 1368865035 1624836578 466928571))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 39856)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 808858814 985041208 1310447599 273298029 1965229540 714283164))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 40258)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 820681809 1875412227 689453514 1614217150 766919052 1584318978))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 41038)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 53802091 1385247068 55710149 1880936786 366779010 2057457079))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 41440)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 569655210 1566909617 380127654 1912395989 1397945266 1915938746))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 41842)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1879059714 1682603721 391984614 667484173 1676019962 916997328))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 46407)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 616579558 566192515 1666544660 822243002 1643465641 2091669444))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 46809)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1557531674 125838207 483190846 1483370483 622589701 258163769))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 47211)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (no-operation) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 375813055 1980190123 550934883 1657146843 622292706 1415264365))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 47613)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1051443614 1443641410 542235364 1692606248 627525294 941874515))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 48393)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 144939847 2057168931 1249337363 1091346660 467583081 755851222))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 49173)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 466672922 2123603539 356858058 1784333141 1688457714 470781299))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 49578)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 696664213 44684830 1324446482 1422798778 569318533 1050767617))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 49980)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1557642071 332954110 621935986 767825251 316008684 1699061073))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 50382)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 363403698 2114693516 2058919360 944660615 252081187 157687627))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 50789)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep annabelle)
       (do (eat annabelle) (do (celebrate annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-04)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1600807355 289431900 236189504 826773301 190899725 1290216131))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 51196)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (no-operation)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-05)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1148809811 1159496454 1667531065 777332014 1436977396 1297526670))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 51603)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 985089078 2089275922 566188395 1686109262 479221855 2036371628))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 52008)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1971120133 71347479 2042301522 1987263219 828581636 18963351))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 52792)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 803840058 1107069037 1973392796 652110971 1977820383 561504533))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 53576)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-07)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1743914561 783848753 315602949 1258364295 1835483644 348552744))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 54360)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-08)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2047705063 209561995 1039381040 437070507 492159247 760633709))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 55145)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (sleep annabelle) (do (work annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2133154663 2013966530 399479872 1225662975 1887373868 190177417))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 55551)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 821627475 2050887834 558787743 608635810 1943181431 1577080872))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 55959)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1403271044 120859171 861082999 2132356060 2064752309 919382352))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 56745)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1111153916 1798793991 493529877 1390573483 2003541435 215118018))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 57530)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.3)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 141170598 1076620525 59471054 120028309 1510743448 1062614513))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 58314)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.4)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 251483986 1440752945 996513048 1251968451 4489666 1055891120))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 58719)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.5)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 27534404 1874009897 1092944679 2119515753 2139292698 2079863350))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 59124)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (work laith) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.6)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2012210418 852846111 2089721205 1170687383 1259409973 514811871))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 59532)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 799826092 754334128 1445320155 1267784157 1705047419 873828945))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 60312)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 4329376 57661585 1176598342 196450331 2093926922 1655183311))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 60720)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1901925597 885085234 1479767094 1249211368 1839141616 2086483851))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 61128)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do (eat #:?sv0.person) s0) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1679572743 547678993 847420842 256639879 235188287 1556467130))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 61538)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (eat annabelle) (do (eat annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 536471546 2035308108 63923838 3157058 1420170121 410663621))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 61947)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (sleep #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (work #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (sleep #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (work #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (sleep #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (work #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (sleep #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (work #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (eat #:?sv0.person)
                                                (do
                                                 (sleep
                                                  #:?sv0.person)
                                                 (do
                                                  (eat #:?sv0.person)
                                                  (do
                                                   (work
                                                    #:?sv0.person)
                                                   (do
                                                    (eat
                                                     #:?sv0.person)
                                                    (do
                                                     (eat
                                                      #:?sv0.person)
                                                     (do
                                                      (sleep
                                                       #:?sv0.person)
                                                      (do
                                                       (eat
                                                        #:?sv0.person)
                                                       (do
                                                        (work
                                                         #:?sv0.person)
                                                        (do
                                                         (eat
                                                          #:?sv0.person)
                                                         (do
                                                          (eat
                                                           #:?sv0.person)
                                                          (do
                                                           (sleep
                                                            #:?sv0.person)
                                                           (do
                                                            (eat
                                                             #:?sv0.person)
                                                            (do
                                                             (work
                                                              #:?sv0.person)
                                                             (do
                                                              (eat
                                                               #:?sv0.person)
                                                              (do
                                                               (eat
                                                                #:?sv0.person)
                                                               (do
                                                                (sleep
                                                                 #:?sv0.person)
                                                                (do
                                                                 (eat
                                                                  #:?sv0.person)
                                                                 (do
                                                                  (work
                                                                   #:?sv0.person)
                                                                  (do
                                                                   (eat
                                                                    #:?sv0.person)
                                                                   (do
                                                                    (eat
                                                                     #:?sv0.person)
                                                                    (do
                                                                     (sleep
                                                                      #:?sv0.person)
                                                                     (do
                                                                      (eat
                                                                       #:?sv0.person)
                                                                      (do
                                                                       (work
                                                                        #:?sv0.person)
                                                                       (do
                                                                        (eat
                                                                         #:?sv0.person)
                                                                        (do
                                                                         (eat
                                                                          #:?sv0.person)
                                                                         (do
                                                                          (sleep
                                                                           #:?sv0.person)
                                                                          (do
                                                                           (eat
                                                                            #:?sv0.person)
                                                                           (do
                                                                            (work
                                                                             #:?sv0.person)
                                                                            (do
                                                                             (eat
                                                                              #:?sv0.person)
                                                                             (do
                                                                              (eat
                                                                               #:?sv0.person)
                                                                              (do
                                                                               (sleep
                                                                                #:?sv0.person)
                                                                               (do
                                                                                (eat
                                                                                 #:?sv0.person)
                                                                                (do
                                                                                 (work
                                                                                  #:?sv0.person)
                                                                                 (do
                                                                                  (eat
                                                                                   #:?sv0.person)
                                                                                  s0 )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09g)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1439477849 2113578315 2063745638 1213113415 1117521470 1839912048))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 62412)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle)
            (do
             (eat annabelle)
             (do (eat annabelle)
                 (do
                  (eat annabelle)
                  (do (eat annabelle)
                      (do
                       (eat annabelle)
                       (do (eat annabelle)
                           (do
                            (eat annabelle)
                            (do (eat annabelle)
                                (do
                                 (eat annabelle)
                                 (do
                                  (eat annabelle)
                                  (do
                                   (eat annabelle)
                                   (do
                                    (eat annabelle)
                                    (do
                                     (eat annabelle)
                                     (do
                                      (eat annabelle)
                                      (do
                                       (eat annabelle)
                                       (do
                                        (eat annabelle)
                                        (do
                                         (eat annabelle)
                                         (do
                                          (eat annabelle)
                                          (do
                                           (eat annabelle)
                                           s0) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09h)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1248592437 1060849750 611678825 1299312802 1274855941 316491153))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 62838)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (eat #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (eat #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (eat #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (eat #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (eat #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (eat #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (eat #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (eat #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (eat #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (eat #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (sleep #:?sv0.person)
                                                s0 )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09i)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 164024817 1197862779 1588179335 199962922 366370821 1727370217))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 63269)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09j)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1788797952 1504907401 618376681 948403748 1534461309 607843570))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 64084)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle)
            (do (eat annabelle) (do (eat annabelle) s0 )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 63407492 1629586979 1636526680 242481500 1766257740 361157122))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 64495)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (work matthias)
       (do (celebrate matthias) (do (sleep matthias) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 716133368 99376539 874522101 1964238326 247000442 1813318306))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 65283)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1555321207 140929103 459002784 245360656 653929758 687328001))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 67588)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 58505531 1995303416 1747957502 1181716493 278161753 1903365859))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 68376)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do (work laith) (do (celebrate laith) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-11)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1383486220 389009927 2080535328 1801106495 241206548 1870463585))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 69919)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person) (do (sleep #:?sv0.person) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1342404111 404173939 137215611 1628900365 65944088 2016103357))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 70326)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1596248587 2071949706 317538894 1555205081 1445210568 2143329122))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 70733)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1702330961 1670174926 2131797812 495124676 1259325952 1240106582))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 71139)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1647315155 1804597613 1874940540 213455713 372856663 478541770))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 71545)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 674112056 1429630940 72560016 1239735623 398673911 1519187658))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 72712)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1037127686 1876511899 384378692 1400972109 98669500 1636410341))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 73497)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 292301922 283568641 1216754264 1520158913 620993580 330685511))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 73904)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 548742926 1718332606 84515351 1654456128 1905876934 2071953383))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 74311)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep matthias) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 909457704 2066091317 1332523208 1836120126 1854468035 480854377))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 74717)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep #:?p74742.person) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 255791369 1744331984 1039509116 1593195086 322805994 82885877))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 75120)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-15)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2050397759 1321683115 701042685 1066870790 2106909721 1371669499))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 75901)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1062435809 702873197 342243235 1479233406 1164733167 145506797))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 77822)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1826959593 443597455 611576903 687384041 166913697 792033913))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 81260)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1630260437 1634768207 678817255 417419217 739962625 1065052476))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 86218)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1507148366 1435495878 464131677 2062901109 833097634 295553429))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 86620)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 252587296 1201462988 734093452 932434026 1447159577 1036793304))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 87022)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1744721449 1869247706 1340533669 1677123396 1362766975 1108791219))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 87424)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 445062648 178927828 441669016 1377647329 1858525612 49728980))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 88204)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2032520795 1706511119 1403036748 1056642218 1004615091 1277246388))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 88606)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2058450435 691626256 552057450 936120342 500687643 2008555194))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 89008)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2060829718 1051444044 1572363182 1447139875 1344222734 2103685529))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 93573)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1633091763 651229121 1873756536 596939905 412473582 1414247037))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 93975)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 47053100 1516420783 458432561 1946274510 1706561765 1514348025))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 94377)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (no-operation) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1032606720 1405963131 429038527 39886592 1780990716 1414976054))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 94779)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 620495218 1843051082 976453553 1285659447 743583039 2146775279))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 95559)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-01e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 127796359 504825412 1830124611 1181483882 1194528294 2090780297))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 96339)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 273140535 1974315922 275193755 562784540 888775315 677222267))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 96744)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-02a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 383500895 1340859119 648478470 125907400 363264254 814726437))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 97146)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate lenz) (do (sleep lenz) (do (eat lenz) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 858363569 1018775104 1550386852 1438552443 1749460072 1544455658))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 97548)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-03a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 621921181 1772924753 1766242397 24222017 633728213 284786137))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 97955)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep annabelle)
       (do (eat annabelle) (do (celebrate annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-04)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 307843035 678178277 243095325 625863272 462807638 1952008307))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 98362)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (no-operation)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-05)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1586805825 1691799094 152303457 690205220 1825857152 127448752))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 98769)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1505624783 2094561042 750542386 121490061 1935930955 804481680))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 99174)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 535256737 798412191 1947449458 826140514 370613250 1692775105))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 99958)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-06b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 451426321 991770752 862241194 1964523205 1001303391 2136400120))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 100742)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  ':nobody-can-work-and-celebrate)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-07)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 96973753 488550328 24131169 31737958 456595816 1480295182))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 101526)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-08)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 433309146 159355358 970168435 1525043193 110350356 603098690))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 102311)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (sleep annabelle) (do (work annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 840471908 830825647 2082846811 1819191036 585843197 51070061))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 102717)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 715865956 1008791174 925702612 1971562021 2064106201 740157086))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 103125)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1210390552 1908768623 1171438380 1139997906 1081727182 884475420))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 103911)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1764507811 430426833 1568431484 1296699412 1429916651 414424123))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 104696)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.3)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1324369073 1532395736 1438701698 491250901 1439879900 1263046647))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 105480)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.4)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1803448468 19605682 1681353303 995248249 453966506 385402191))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 105885)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.5)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2073365926 184183892 1986869659 1525823496 1014168901 302930289))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 106290)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) (do (work laith) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09a.6)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 747037538 2024157720 497244764 1726449582 1483392735 348538401))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 106698)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 912573221 709730545 1808742916 168561644 1759836349 986573662))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 107478)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (eat #:?sv0.person) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1344238137 71483678 42183514 1719649716 1686535937 797659919))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 107886)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (eat annabelle) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 70313152 932526423 2048128891 247906080 25969899 1846139939))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 108294)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do (eat #:?sv0.person) s0) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1782515190 2002699370 1819330157 541796944 726121134 69219318))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 108704)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do (eat annabelle) (do (eat annabelle) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1426245436 1362122283 678507508 575921778 395960585 1986716603))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 109113)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (work #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (sleep #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (work #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (sleep #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (work #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (sleep #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (work #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (sleep #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (work #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (eat #:?sv0.person)
                                                (do
                                                 (sleep
                                                  #:?sv0.person)
                                                 (do
                                                  (eat #:?sv0.person)
                                                  (do
                                                   (work
                                                    #:?sv0.person)
                                                   (do
                                                    (eat
                                                     #:?sv0.person)
                                                    (do
                                                     (eat
                                                      #:?sv0.person)
                                                     (do
                                                      (sleep
                                                       #:?sv0.person)
                                                      (do
                                                       (eat
                                                        #:?sv0.person)
                                                       (do
                                                        (work
                                                         #:?sv0.person)
                                                        (do
                                                         (eat
                                                          #:?sv0.person)
                                                         (do
                                                          (eat
                                                           #:?sv0.person)
                                                          (do
                                                           (sleep
                                                            #:?sv0.person)
                                                           (do
                                                            (eat
                                                             #:?sv0.person)
                                                            (do
                                                             (work
                                                              #:?sv0.person)
                                                             (do
                                                              (eat
                                                               #:?sv0.person)
                                                              (do
                                                               (eat
                                                                #:?sv0.person)
                                                               (do
                                                                (sleep
                                                                 #:?sv0.person)
                                                                (do
                                                                 (eat
                                                                  #:?sv0.person)
                                                                 (do
                                                                  (work
                                                                   #:?sv0.person)
                                                                  (do
                                                                   (eat
                                                                    #:?sv0.person)
                                                                   (do
                                                                    (eat
                                                                     #:?sv0.person)
                                                                    (do
                                                                     (sleep
                                                                      #:?sv0.person)
                                                                     (do
                                                                      (eat
                                                                       #:?sv0.person)
                                                                      (do
                                                                       (work
                                                                        #:?sv0.person)
                                                                       (do
                                                                        (eat
                                                                         #:?sv0.person)
                                                                        (do
                                                                         (eat
                                                                          #:?sv0.person)
                                                                         (do
                                                                          (sleep
                                                                           #:?sv0.person)
                                                                          (do
                                                                           (eat
                                                                            #:?sv0.person)
                                                                           (do
                                                                            (work
                                                                             #:?sv0.person)
                                                                            (do
                                                                             (eat
                                                                              #:?sv0.person)
                                                                             (do
                                                                              (eat
                                                                               #:?sv0.person)
                                                                              (do
                                                                               (sleep
                                                                                #:?sv0.person)
                                                                               (do
                                                                                (eat
                                                                                 #:?sv0.person)
                                                                                (do
                                                                                 (work
                                                                                  #:?sv0.person)
                                                                                 (do
                                                                                  (eat
                                                                                   #:?sv0.person)
                                                                                  s0 )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09g)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 903192271 708788237 1871349146 1313896747 439180400 1687731265))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 109578)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle)
            (do
             (eat annabelle)
             (do (eat annabelle)
                 (do
                  (eat annabelle)
                  (do (eat annabelle)
                      (do
                       (eat annabelle)
                       (do (eat annabelle)
                           (do
                            (eat annabelle)
                            (do (eat annabelle)
                                (do
                                 (eat annabelle)
                                 (do
                                  (eat annabelle)
                                  (do
                                   (eat annabelle)
                                   (do
                                    (eat annabelle)
                                    (do
                                     (eat annabelle)
                                     (do
                                      (eat annabelle)
                                      (do
                                       (eat annabelle)
                                       (do
                                        (eat annabelle)
                                        (do
                                         (eat annabelle)
                                         (do
                                          (eat annabelle)
                                          (do
                                           (eat annabelle)
                                           s0) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09h)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1258774352 2009880376 756073445 1939618207 1956483687 948510397))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 110004)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (eat #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (eat #:?sv0.person)
                 (do
                  (eat #:?sv0.person)
                  (do (eat #:?sv0.person)
                      (do
                       (eat #:?sv0.person)
                       (do (eat #:?sv0.person)
                           (do
                            (eat #:?sv0.person)
                            (do (eat #:?sv0.person)
                                (do
                                 (eat #:?sv0.person)
                                 (do
                                  (eat #:?sv0.person)
                                  (do
                                   (eat #:?sv0.person)
                                   (do
                                    (eat #:?sv0.person)
                                    (do
                                     (eat #:?sv0.person)
                                     (do
                                      (eat #:?sv0.person)
                                      (do
                                       (eat #:?sv0.person)
                                       (do
                                        (eat #:?sv0.person)
                                        (do
                                         (eat #:?sv0.person)
                                         (do
                                          (eat #:?sv0.person)
                                          (do
                                           (eat #:?sv0.person)
                                           (do
                                            (eat #:?sv0.person)
                                            (do
                                             (eat #:?sv0.person)
                                             (do
                                              (eat #:?sv0.person)
                                              (do
                                               (eat #:?sv0.person)
                                               (do
                                                (sleep #:?sv0.person)
                                                s0 )) )) )) )) )) )) )) )) )) )) )) )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09i)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1692718518 1459750048 105187892 1600070831 1048233965 2133616144))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 110435)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-09j)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1840558069 229337967 1271058261 1924313949 1495344433 51170185))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 111250)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (eat #:?sv0.person)
            (do
             (eat #:?sv0.person)
             (do (sleep #:?sv0.person) s0 )) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 362982065 1665605046 702327108 1433233427 472110600 558669211))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 111661)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (work matthias)
       (do (celebrate matthias) (do (sleep matthias) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 286159214 815726690 1863454207 231224235 567408917 1626094219))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 112449)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 994814799 2064778152 331304645 67805387 190673865 1317096236))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 114754)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do
        (work laith)
        (do (celebrate laith) (do (sleep laith) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-10c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 263670881 1580485583 598876887 1380660269 2053671124 1906059156))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 115542)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (hold-family-meeting laith)
       (do (work laith) (do (celebrate laith) s0 )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-11)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 769804133 350332756 92994609 1975026320 1737180788 840839410))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 117085)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1117951824 2077794024 580484195 720753423 2057066490 1746289055))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 117492)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person) (do (sleep #:?sv0.person) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1126753062 225890322 1375648954 1439298951 278138769 153131527))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 117899)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-12b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 290386691 134889993 1082394425 292849724 451298787 1851279817))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 118305)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1008337277 655789963 634136542 1517170834 612316346 1648524587))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 118711)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 111016597 1731758321 915193449 1585036825 1756469875 1218630135))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 119120)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1667445382 12433907 869320730 2120124415 1627114256 285000038))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 119905)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle)
       (do
        (eat annabelle)
        (do (eat annabelle) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-13c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1230523105 1622107362 1570834778 1350659296 1043121716 1387783947))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 120312)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate #:?sv0.person)
       (do
        (eat #:?sv0.person)
        (do (sleep #:?sv0.person) (do (sleep matthias) s0) )) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 796383317 1962021141 865903322 1469099443 158311831 422708172))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 120719)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep matthias) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 2101496896 562624530 1147415694 2003951774 325953503 375184810))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 121125)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (sleep #:?p121150.person) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-14b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1366355983 1720202435 740962908 2047437556 1001614923 1901186584))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 121528)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-15)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1469986900 340738404 1696856903 700813141 1709502520 1555961772))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 122309)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate annabelle) s0 ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1802404719 676055339 1935351565 4475406 607830847 846813293))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 124230)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  '(do (celebrate laith) (do (work annabelle) s0) ))

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16a)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 814494572 264978959 351176588 1890681619 326845367 1102658755))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 128806)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16b)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1325330738 1791311951 267702468 1338245512 354457427 1905891318))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 133764)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16c)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1242162568 2038594769 730147739 1378566976 1599464763 65124683))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 134166)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 866538346 334674305 1615556572 743874534 705482105 1233323480))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 134568)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.1)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1901108380 1295700832 739947014 708068486 1073360260 173379538))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 134970)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16d.2)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1855631240 1649786733 886796035 674735175 1899788880 868115251))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 135750)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16e)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 1904018963 2002982723 1200306354 1672267737 1140344957 1552674721))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 136152)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  's0)

#+ccl
(make-instance
  'example-test-case
  :example
  (find-example 'interpret-16f)
  :lisp-implementation-type
  "Clozure Common Lisp"
  :lisp-implementation-version
  "Version 1.8-r15378M  (DarwinX8664)"
  :random-state
  (make-random-state
    #.(ccl::initialize-mrg31k3p-state 997688265 1449352623 847655189 1764212219 1785507404 2103783871))
  :variable-values
  '((odysseus::*store-all-non-refuted-proof-terms*)
    (*continue-after-undecidable-precondition*)
    (*continue-after-undecidable-test* . t)
    (odysseus::*optimize-interpretation-of-declarations* . t)
    (*permute-offline-choice* . t)
    (odysseus::*instantiate-undecidable-choices* . t)
    (odysseus::*assert-rewrite-for-declarations*)
    (odysseus::*assert-rewrite-for-preconditions* . t)
    (odysseus::*assert-rewrite-for-unique-names-axioms*)
    (odysseus::*support-declarations*)
    (odysseus::*support-preconditions*)
    (odysseus::*support-unique-name-axioms*)
    (odysseus::*unique-variable-counter* . 136554)
    (odysseus-snark::*run-time-limit* . 0.2)
    (odysseus-snark::*ida-run-time-limit* . 0.1)
    (odysseus-snark::*ida-iterations* . 3)
    (odysseus-snark::*error-when-refutation-without-answer*))
  :execution-mode
  :offline
  :expected-result
  'no-next-choice-point)

