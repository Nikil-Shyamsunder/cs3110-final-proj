open OUnit2
open Prescription_validator.Account
open Prescription_validator.Authenticator
open Prescription_validator.Doctor
open Prescription_validator.Patient
open Prescription_validator.Pharmacist
open Prescription_validator.Task
open Prescription_validator

let create_doctor _ =   let doctor = Doctor.create_user "Dr. Smith" "password123" "doctor" [] in
assert_equal "doctor" (role doctor);


let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main tests
