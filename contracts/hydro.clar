;; Hydro Commerce - Advanced Resource Allocation Protocol For Liquid Assets
;; A decentralized marketplace for trading allocated consumption units and enables trustless exchange of verified consumption rights between participants

;; Record-Keeping Structures
(define-map participant-unit-holdings principal uint)
(define-map participant-token-holdings principal uint)
(define-map marketplace-listings {participant: principal} {quantity: uint, unit-price: uint})

;; Internal Utility Functions

;; Calculate exchange commission
(define-private (determine-commission-amount (transaction-value uint))
  (/ (* transaction-value (var-get marketplace-fee-rate)) u100))

;; Calculate exit compensation
(define-private (calculate-exit-compensation (quantity uint))
  (/ (* quantity (var-get base-unit-value) (var-get exit-compensation-rate)) u100))

;; Modify ecosystem allocation tracking
(define-private (adjust-ecosystem-allocation (quantity-change int))
  (let (
    (current-allocation (var-get ecosystem-current-allocation))
    (adjusted-allocation (if (< quantity-change 0)
                     (if (>= current-allocation (to-uint (- 0 quantity-change)))
                         (- current-allocation (to-uint (- 0 quantity-change)))
                         u0)
                     (+ current-allocation (to-uint quantity-change))))
  )
    (asserts! (<= adjusted-allocation (var-get ecosystem-capacity-maximum)) status-system-capacity-breach)
    (var-set ecosystem-current-allocation adjusted-allocation)
    (ok true)))


;; System Configuration Parameters
(define-constant admin-account tx-sender)
(define-constant status-unauthorized (err u100))
(define-constant status-insufficient-units (err u101))
(define-constant status-exchange-failed (err u102))
(define-constant status-invalid-unit-cost (err u103))
(define-constant status-zero-quantity (err u104))
(define-constant status-invalid-commission (err u105))
(define-constant status-reimbursement-failed (err u106))
(define-constant status-self-transaction (err u107))
(define-constant status-system-capacity-breach (err u108))
(define-constant status-limit-violation (err u109))

;; System State Variables
(define-data-var base-unit-value uint u100) ;; Standard unit price in microstacks (1 STX = 1,000,000 microstacks)
(define-data-var individual-allocation-ceiling uint u10000) ;; Maximum allocation per participant
(define-data-var marketplace-fee-rate uint u5) ;; Fee percentage charged on exchanges
(define-data-var exit-compensation-rate uint u90) ;; Compensation percentage for unit returns
(define-data-var ecosystem-capacity-maximum uint u1000000) ;; Maximum system-wide allocation limit
(define-data-var ecosystem-current-allocation uint u0) ;; Current total allocation in the system


;; Marketplace Participant Functions

;; List units in marketplace
(define-public (list-units-for-exchange (quantity uint) (unit-price uint))
  (let (
    (participant-balance (default-to u0 (map-get? participant-unit-holdings tx-sender)))
    (existing-listing-quantity (get quantity (default-to {quantity: u0, unit-price: u0} 
                               (map-get? marketplace-listings {participant: tx-sender}))))
    (total-listing-quantity (+ quantity existing-listing-quantity))
  )
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (> unit-price u0) status-invalid-unit-cost)
    (asserts! (>= participant-balance total-listing-quantity) status-insufficient-units)
    (try! (adjust-ecosystem-allocation (to-int quantity)))
    (map-set marketplace-listings {participant: tx-sender} 
             {quantity: total-listing-quantity, unit-price: unit-price})
    (ok true)))

;; Acquire units from another participant
(define-public (acquire-listed-units (provider principal) (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, unit-price: u0} 
                  (map-get? marketplace-listings {participant: provider})))
    (acquisition-cost (* quantity (get unit-price listing-data)))
    (marketplace-commission (determine-commission-amount acquisition-cost))
    (total-transaction-cost (+ acquisition-cost marketplace-commission))
    (provider-balance (default-to u0 (map-get? participant-unit-holdings provider)))
    (acquirer-tokens (default-to u0 (map-get? participant-token-holdings tx-sender)))
    (provider-tokens (default-to u0 (map-get? participant-token-holdings provider)))
    (admin-tokens (default-to u0 (map-get? participant-token-holdings admin-account)))
  )
    (asserts! (not (is-eq tx-sender provider)) status-self-transaction)
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (>= (get quantity listing-data) quantity) status-insufficient-units)
    (asserts! (>= provider-balance quantity) status-insufficient-units)
    (asserts! (>= acquirer-tokens total-transaction-cost) status-insufficient-units)

    ;; Update provider's unit balance and listing
    (map-set participant-unit-holdings provider (- provider-balance quantity))
    (map-set marketplace-listings {participant: provider} 
             {quantity: (- (get quantity listing-data) quantity), 
              unit-price: (get unit-price listing-data)})

    ;; Update acquirer's token and unit holdings
    (map-set participant-token-holdings tx-sender (- acquirer-tokens total-transaction-cost))
    (map-set participant-unit-holdings tx-sender 
             (+ (default-to u0 (map-get? participant-unit-holdings tx-sender)) quantity))

    ;; Distribute tokens to provider and admin
    (map-set participant-token-holdings provider (+ provider-tokens acquisition-cost))
    (map-set participant-token-holdings admin-account (+ admin-tokens marketplace-commission))

    (ok true)))

;; Return units for compensation
(define-public (return-units-for-compensation (quantity uint))
  (let (
    (participant-units (default-to u0 (map-get? participant-unit-holdings tx-sender)))
    (compensation-amount (calculate-exit-compensation quantity))
    (admin-token-balance (default-to u0 (map-get? participant-token-holdings admin-account)))
  )
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (>= participant-units quantity) status-insufficient-units)
    (asserts! (>= admin-token-balance compensation-amount) status-reimbursement-failed)

    ;; Update participant's unit balance
    (map-set participant-unit-holdings tx-sender (- participant-units quantity))

    ;; Transfer compensation tokens
    (map-set participant-token-holdings tx-sender 
             (+ (default-to u0 (map-get? participant-token-holdings tx-sender)) compensation-amount))
    (map-set participant-token-holdings admin-account (- admin-token-balance compensation-amount))

    (ok true)))

;; Enhanced unit return with validation
(define-public (verified-unit-return (quantity uint))
  (let (
        (participant-units (default-to u0 (map-get? participant-unit-holdings tx-sender)))
        (compensation-amount (calculate-exit-compensation quantity))
  )
    (asserts! (>= participant-units quantity) status-insufficient-units)
    (asserts! (> compensation-amount u0) status-reimbursement-failed)

    ;; Process the return transaction
    (map-set participant-unit-holdings tx-sender (- participant-units quantity))
    (map-set participant-token-holdings tx-sender 
             (+ (default-to u0 (map-get? participant-token-holdings tx-sender)) compensation-amount))
    (map-set participant-token-holdings admin-account 
             (- (default-to u0 (map-get? participant-token-holdings admin-account)) compensation-amount))

    (ok true)))

;; Streamlined acquisition process
(define-public (expedited-unit-acquisition (provider principal) (quantity uint))
  (let (
        (listing-data (default-to {quantity: u0, unit-price: u0} 
                      (map-get? marketplace-listings {participant: provider})))
        (acquisition-cost (* quantity (get unit-price listing-data)))
        (acquirer-balance (default-to u0 (map-get? participant-token-holdings tx-sender)))
        (provider-units (default-to u0 (map-get? participant-unit-holdings provider)))
  )
    (asserts! (>= acquirer-balance acquisition-cost) status-insufficient-units)
    (asserts! (>= provider-units quantity) status-insufficient-units)

    (map-set participant-token-holdings tx-sender (- acquirer-balance acquisition-cost))
    (map-set participant-unit-holdings tx-sender 
             (+ (default-to u0 (map-get? participant-unit-holdings tx-sender)) quantity))
    (map-set participant-unit-holdings provider (- provider-units quantity))
    (map-set participant-token-holdings provider 
             (+ (default-to u0 (map-get? participant-token-holdings provider)) acquisition-cost))
    (ok true)))

;; Direct unit transfer between participants
(define-public (transfer-units-to-participant (recipient principal) (quantity uint))
  (let (
    (sender-balance (default-to u0 (map-get? participant-unit-holdings tx-sender)))
    (recipient-balance (default-to u0 (map-get? participant-unit-holdings recipient)))
    (transfer-fee (determine-commission-amount (var-get base-unit-value)))
    (sender-token-balance (default-to u0 (map-get? participant-token-holdings tx-sender)))
  )
    (asserts! (not (is-eq tx-sender recipient)) status-self-transaction)
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (>= sender-balance quantity) status-insufficient-units)
    (asserts! (>= sender-token-balance transfer-fee) status-insufficient-units)
    (asserts! (<= (+ recipient-balance quantity) (var-get individual-allocation-ceiling)) status-system-capacity-breach)

    ;; Update unit balances
    (map-set participant-unit-holdings tx-sender (- sender-balance quantity))
    (map-set participant-unit-holdings recipient (+ recipient-balance quantity))

    ;; Process transfer fee
    (map-set participant-token-holdings tx-sender (- sender-token-balance transfer-fee))
    (map-set participant-token-holdings admin-account 
             (+ (default-to u0 (map-get? participant-token-holdings admin-account)) transfer-fee))

    (ok true)
  )
)

;; Register new allocation in system
;; Allocates new units to a participant's account based on verified allocation rights
(define-public (register-new-allocation (quantity uint))
  (let (
    (participant-balance (default-to u0 (map-get? participant-unit-holdings tx-sender)))
    (updated-balance (+ participant-balance quantity))
    (current-system-allocation (var-get ecosystem-current-allocation))
    (updated-system-allocation (+ current-system-allocation quantity))
  )
    ;; Verify quantity is valid
    (asserts! (> quantity u0) status-zero-quantity)
    ;; Verify participant's allocation limit
    (asserts! (<= updated-balance (var-get individual-allocation-ceiling)) status-system-capacity-breach)
    ;; Verify system-wide allocation limit
    (asserts! (<= updated-system-allocation (var-get ecosystem-capacity-maximum)) status-system-capacity-breach)
    ;; Update participant's allocation record
    (map-set participant-unit-holdings tx-sender updated-balance)
    ;; Update system allocation tracking
    (var-set ecosystem-current-allocation updated-system-allocation)
    ;; Return success status
    (ok true)))

;; Remove units from marketplace
;; Allows participants to delist their units without compensation
(define-public (delist-marketplace-units (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, unit-price: u0} 
                  (map-get? marketplace-listings {participant: tx-sender})))
    (listed-quantity (get quantity listing-data))
    (listed-price (get unit-price listing-data))
  )
    ;; Verify quantity is valid
    (asserts! (> quantity u0) status-zero-quantity)
    ;; Verify sufficient units are listed
    (asserts! (>= listed-quantity quantity) status-insufficient-units)
    ;; Update the marketplace listings
    (map-set marketplace-listings 
             {participant: tx-sender} 
             {quantity: (- listed-quantity quantity), unit-price: listed-price})

    (ok true)))

;; Token withdrawal function
(define-public (withdraw-tokens (quantity uint))
  (let (
    (participant-balance (default-to u0 (map-get? participant-token-holdings tx-sender)))
    (new-balance (if (>= participant-balance quantity)
                    (- participant-balance quantity)
                    u0))
  )
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (>= participant-balance quantity) status-insufficient-units)

    ;; Update participant token record
    (map-set participant-token-holdings tx-sender new-balance)

    ;; Execute token transfer
    (try! (as-contract (stx-transfer? quantity (as-contract tx-sender) tx-sender)))

    (ok new-balance)))

;; Administrative allocation function
(define-public (administrative-allocation (participant principal) (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? participant-unit-holdings participant)))
    (new-balance (+ current-balance quantity))
    (system-allocation (var-get ecosystem-current-allocation))
    (updated-system-allocation (+ system-allocation quantity))
  )
    (asserts! (is-eq tx-sender admin-account) status-unauthorized)
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (<= new-balance (var-get individual-allocation-ceiling)) status-system-capacity-breach)
    (asserts! (<= updated-system-allocation (var-get ecosystem-capacity-maximum)) status-system-capacity-breach)

    ;; Update system allocation
    (var-set ecosystem-current-allocation updated-system-allocation)

    ;; Record the allocation event
    (print {event: "admin-allocation", participant: participant, quantity: quantity, new-balance: new-balance})

    (ok new-balance)))

;; Delist all participant units from marketplace
;; Provides complete removal of marketplace listings
(define-public (withdraw-marketplace-listing (quantity uint))
  (let (
    (current-listing (default-to {quantity: u0, unit-price: u0} 
                     (map-get? marketplace-listings {participant: tx-sender})))
    (listing-quantity (get quantity current-listing))
    (listing-unit-price (get unit-price current-listing))
  )
    (asserts! (> quantity u0) status-zero-quantity)
    (asserts! (>= listing-quantity quantity) status-insufficient-units)

    ;; Update or remove the marketplace listing
    (if (is-eq listing-quantity quantity)
        (map-delete marketplace-listings {participant: tx-sender})
        (map-set marketplace-listings {participant: tx-sender} 
                {quantity: (- listing-quantity quantity), unit-price: listing-unit-price}))

    (ok true)))

;; Complete removal of marketplace listing
(define-public (terminate-marketplace-participation)
  (let (
    (listing-data (default-to {quantity: u0, unit-price: u0} 
                  (map-get? marketplace-listings {participant: tx-sender})))
    (listed-quantity (get quantity listing-data))
    (system-allocation (var-get ecosystem-current-allocation))
  )
    (asserts! (> listed-quantity u0) status-insufficient-units)

    ;; Update system allocation tracking
    (var-set ecosystem-current-allocation (- system-allocation listed-quantity))

    ;; Remove the marketplace listing completely
    (map-set marketplace-listings {participant: tx-sender} {quantity: u0, unit-price: u0})

    ;; Log the marketplace exit event
    (print {event: "marketplace-exit", participant: tx-sender, quantity: listed-quantity})

    (ok true)))

