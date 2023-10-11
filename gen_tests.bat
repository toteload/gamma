@echo off

cd tests
python generate_sample_tests.py > test_valid_samples.rs
