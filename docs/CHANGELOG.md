# Changelog

All notable changes to the uada80 project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Enumeration Type Support** (Complete):
  - Enumeration type declarations
  - Enumeration literals as constants
  - Comparison operations on enums
  - Enums in control flow (if/case statements)
  - Enums as parameters and return values
  - Predefined Boolean and Character types
  - Comprehensive test suite (22 tests, all passing)
  - Full documentation in ENUMS_IMPLEMENTATION.md
  - Automatic symbol table registration for literals
- **Record Type Support** (Complete):
  - Record type declarations
  - Field assignment and reading
  - Nested records
  - Mixed field types (Integer, Character, Boolean)
  - Records as parameters and return values
  - Comprehensive test suite (15 tests, all passing)
  - Full documentation in RECORDS_IMPLEMENTATION.md
- **CP/M Target Platform Specification** (Complete):
  - CPM_RUNTIME.md - Complete Ada/CP/M runtime library specification
  - CPM_QUICK_REFERENCE.md - CP/M quick reference for developers
  - cpm22_bdos_calls.pdf - BDOS system call reference
  - cpm22_bios_calls.pdf - BIOS hardware interface reference
  - cpm22_memory_layout.pdf - CP/M memory organization
  - SESSION_2025_12_06.md - Development session summary
- Initial project structure
- Project documentation:
  - ARCHITECTURE.md - Compiler architecture design
  - AST_DESIGN.md - Abstract syntax tree structure
  - OPTIMIZATION_ANALYSIS.md - Analysis of reusable optimization code from uplm80
  - LANGUAGE_SUBSET.md - Ada language subset implementation plan
  - README.md - Project overview and getting started guide
  - PLAN.md - Development plan
  - CHANGELOG.md - This file
- Downloaded Ada 2012 Reference Manual (specs/RM-2012.pdf)
- Downloaded ACATS test suite (acats/)
- Build system (pyproject.toml)
- Python package structure (uada80/)
- Example Ada programs:
  - hello.ada - Hello World
  - fibonacci.ada - Fibonacci sequence
  - factorial.ada - Factorial calculator
- Development infrastructure:
  - .gitignore
  - Test directory structure
  - Optimizer and codegen subdirectories

### Changed
- Organized documentation into docs/ directory
- Updated LANGUAGE_SUBSET.md to specify CP/M 2.2 as target platform
- Updated README.md to emphasize CP/M targeting
- Clarified Z80/CP/M limitations in documentation

### Deprecated
- None

### Removed
- None

### Fixed
- Enumeration literals now properly added to symbol table during type declaration
- Type system correctly handles enum position values

### Security
- None

## [0.1.0] - 2025-12-06

### Added
- Initial project setup and planning
- Comprehensive architectural design
- Language subset definition for phased implementation
- Analysis of optimization techniques from uplm80 project

---

## Project Milestones

### Phase 1: MVP ✅
- [x] Lexer implementation
- [x] Parser implementation (basic subset)
- [x] AST node definitions
- [x] Basic semantic analysis
- [x] Simple code generator
- [x] Compile "Hello World"

### Phase 2: Expanded Features ✅
- [x] Full semantic analysis
- [x] Package support
- [x] Access types
- [x] Enumeration types
- [x] AST optimization

### Phase 3: ACATS Compliance 🔧
- [x] Generic support
- [x] Exception handling
- [x] Full attribute support
- [ ] Pass core ACATS tests
- [ ] Standard library for Z80
- [ ] Performance tuning
