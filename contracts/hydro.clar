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
