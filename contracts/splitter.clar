;; ===================================================================
;; SIMPLE SUBSCRIPTION SPLITTER
;; A shared service cost management system for streaming/software subscriptions
;; ===================================================================

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_INVALID_PERIOD (err u105))
(define-constant ERR_SUBSCRIPTION_INACTIVE (err u106))
(define-constant ERR_ALREADY_MEMBER (err u107))
(define-constant ERR_NOT_MEMBER (err u108))
(define-constant ERR_ROTATION_NOT_DUE (err u109))
(define-constant ERR_INVALID_SHARE (err u110))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var subscription-counter uint u0)

;; Data maps
(define-map subscriptions
    { subscription-id: uint }
    {
        name: (string-ascii 64),
        cost-per-period: uint,
        period-blocks: uint,
        owner: principal,
        account-holder: principal,
        active: bool,
        created-at: uint,
        next-rotation: uint,
        next-payment: uint
    }
)

(define-map subscription-members
    { subscription-id: uint, member: principal }
    {
        share-percentage: uint,
        total-contributed: uint,
        usage-count: uint,
        joined-at: uint,
        active: bool
    }
)

(define-map member-balances
    { subscription-id: uint, member: principal }
    { balance: uint }
)

(define-map subscription-stats
    { subscription-id: uint }
    {
        total-members: uint,
        total-contributions: uint,
        total-payments: uint,
        rotation-count: uint
    }
)

;; Read-only functions

(define-read-only (get-subscription (subscription-id uint))
    (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-member-info (subscription-id uint) (member principal))
    (map-get? subscription-members { subscription-id: subscription-id, member: member })
)

(define-read-only (get-member-balance (subscription-id uint) (member principal))
    (default-to u0 (get balance (map-get? member-balances { subscription-id: subscription-id, member: member })))
)

(define-read-only (get-subscription-stats (subscription-id uint))
    (map-get? subscription-stats { subscription-id: subscription-id })
)

(define-read-only (calculate-member-cost (subscription-id uint) (member principal))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (member-info (unwrap! (get-member-info subscription-id member) ERR_NOT_MEMBER))
        (cost-per-period (get cost-per-period subscription))
        (share-percentage (get share-percentage member-info))
    )
        (ok (/ (* cost-per-period share-percentage) u100))
    )
)

(define-read-only (is-payment-due (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (next-payment (get next-payment subscription))
    )
        (ok (>= stacks-block-height next-payment))
    )
)

(define-read-only (is-rotation-due (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (next-rotation (get next-rotation subscription))
    )
        (ok (>= stacks-block-height next-rotation))
    )
)

(define-read-only (get-contract-owner)
    (var-get contract-owner)
)

;; Public functions

(define-public (create-subscription
    (name (string-ascii 64))
    (cost-per-period uint)
    (period-blocks uint)
    (rotation-blocks uint))
    (let (
        (subscription-id (+ (var-get subscription-counter) u1))
        (current-block stacks-block-height)
    )
        (asserts! (> cost-per-period u0) ERR_INVALID_AMOUNT)
        (asserts! (> period-blocks u0) ERR_INVALID_PERIOD)

        ;; Create subscription
        (map-set subscriptions
            { subscription-id: subscription-id }
            {
                name: name,
                cost-per-period: cost-per-period,
                period-blocks: period-blocks,
                owner: tx-sender,
                account-holder: tx-sender,
                active: true,
                created-at: current-block,
                next-rotation: (+ current-block rotation-blocks),
                next-payment: (+ current-block period-blocks)
            }
        )

        ;; Initialize stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            {
                total-members: u0,
                total-contributions: u0,
                total-payments: u0,
                rotation-count: u0
            }
        )

        ;; Add creator as member with 100% share initially
        (map-set subscription-members
            { subscription-id: subscription-id, member: tx-sender }
            {
                share-percentage: u100,
                total-contributed: u0,
                usage-count: u0,
                joined-at: current-block,
                active: true
            }
        )

        ;; Update stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge (unwrap-panic (get-subscription-stats subscription-id))
                   { total-members: u1 })
        )

        (var-set subscription-counter subscription-id)
        (ok subscription-id)
    )
)

(define-public (join-subscription (subscription-id uint) (share-percentage uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (stats (unwrap! (get-subscription-stats subscription-id) ERR_NOT_FOUND))
        (current-block stacks-block-height)
    )
        (asserts! (get active subscription) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (and (> share-percentage u0) (<= share-percentage u100)) ERR_INVALID_SHARE)
        (asserts! (is-none (get-member-info subscription-id tx-sender)) ERR_ALREADY_MEMBER)

        ;; Add member
        (map-set subscription-members
            { subscription-id: subscription-id, member: tx-sender }
            {
                share-percentage: share-percentage,
                total-contributed: u0,
                usage-count: u0,
                joined-at: current-block,
                active: true
            }
        )

        ;; Initialize balance
        (map-set member-balances
            { subscription-id: subscription-id, member: tx-sender }
            { balance: u0 }
        )

        ;; Update stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge stats { total-members: (+ (get total-members stats) u1) })
        )

        (ok true)
    )
)

(define-public (contribute (subscription-id uint) (amount uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (member-info (unwrap! (get-member-info subscription-id tx-sender) ERR_NOT_MEMBER))
        (current-balance (get-member-balance subscription-id tx-sender))
        (stats (unwrap! (get-subscription-stats subscription-id) ERR_NOT_FOUND))
    )
        (asserts! (get active subscription) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (get active member-info) ERR_NOT_MEMBER)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)

        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

        ;; Update member balance
        (map-set member-balances
            { subscription-id: subscription-id, member: tx-sender }
            { balance: (+ current-balance amount) }
        )

        ;; Update member contribution total
        (map-set subscription-members
            { subscription-id: subscription-id, member: tx-sender }
            (merge member-info { total-contributed: (+ (get total-contributed member-info) amount) })
        )

        ;; Update subscription stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge stats { total-contributions: (+ (get total-contributions stats) amount) })
        )

        (ok true)
    )
)

(define-public (process-payment (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (stats (unwrap! (get-subscription-stats subscription-id) ERR_NOT_FOUND))
        (cost-per-period (get cost-per-period subscription))
        (account-holder (get account-holder subscription))
    )
        (asserts! (get active subscription) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (unwrap! (is-payment-due subscription-id) ERR_NOT_FOUND) ERR_INVALID_PERIOD)

        ;; Only account holder or contract owner can process payment
        (asserts! (or (is-eq tx-sender account-holder)
                     (is-eq tx-sender (var-get contract-owner)))
                 ERR_UNAUTHORIZED)

        ;; Transfer payment to account holder
        (try! (as-contract (stx-transfer? cost-per-period tx-sender account-holder)))

        ;; Update next payment date
        (map-set subscriptions
            { subscription-id: subscription-id }
            (merge subscription
                   { next-payment: (+ (get next-payment subscription) (get period-blocks subscription)) })
        )

        ;; Update stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge stats { total-payments: (+ (get total-payments stats) cost-per-period) })
        )

        (ok true)
    )
)

(define-public (rotate-account-holder (subscription-id uint) (new-holder principal))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (stats (unwrap! (get-subscription-stats subscription-id) ERR_NOT_FOUND))
    )
        (asserts! (get active subscription) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (unwrap! (is-rotation-due subscription-id) ERR_NOT_FOUND) ERR_ROTATION_NOT_DUE)
        (asserts! (is-some (get-member-info subscription-id new-holder)) ERR_NOT_MEMBER)

        ;; Only subscription owner can rotate
        (asserts! (is-eq tx-sender (get owner subscription)) ERR_UNAUTHORIZED)

        ;; Update account holder and next rotation
        (map-set subscriptions
            { subscription-id: subscription-id }
            (merge subscription
                   {
                     account-holder: new-holder,
                     next-rotation: (+ stacks-block-height (get period-blocks subscription))
                   })
        )

        ;; Update rotation count
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge stats { rotation-count: (+ (get rotation-count stats) u1) })
        )

        (ok true)
    )
)

(define-public (track-usage (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (member-info (unwrap! (get-member-info subscription-id tx-sender) ERR_NOT_MEMBER))
    )
        (asserts! (get active subscription) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (get active member-info) ERR_NOT_MEMBER)

        ;; Update usage count
        (map-set subscription-members
            { subscription-id: subscription-id, member: tx-sender }
            (merge member-info { usage-count: (+ (get usage-count member-info) u1) })
        )

        (ok true)
    )
)

(define-public (deactivate-subscription (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender (get owner subscription)) ERR_UNAUTHORIZED)

        (map-set subscriptions
            { subscription-id: subscription-id }
            (merge subscription { active: false })
        )

        (ok true)
    )
)

(define-public (leave-subscription (subscription-id uint))
    (let (
        (subscription (unwrap! (get-subscription subscription-id) ERR_NOT_FOUND))
        (member-info (unwrap! (get-member-info subscription-id tx-sender) ERR_NOT_MEMBER))
        (member-balance (get-member-balance subscription-id tx-sender))
        (stats (unwrap! (get-subscription-stats subscription-id) ERR_NOT_FOUND))
    )
        (asserts! (get active member-info) ERR_NOT_MEMBER)
        (asserts! (not (is-eq tx-sender (get owner subscription))) ERR_UNAUTHORIZED)

        ;; Refund remaining balance
        (if (> member-balance u0)
            (try! (as-contract (stx-transfer? member-balance tx-sender tx-sender)))
            true
        )

        ;; Deactivate member
        (map-set subscription-members
            { subscription-id: subscription-id, member: tx-sender }
            (merge member-info { active: false })
        )

        ;; Clear balance
        (map-set member-balances
            { subscription-id: subscription-id, member: tx-sender }
            { balance: u0 }
        )

        ;; Update stats
        (map-set subscription-stats
            { subscription-id: subscription-id }
            (merge stats { total-members: (- (get total-members stats) u1) })
        )

        (ok true)
    )
)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)
