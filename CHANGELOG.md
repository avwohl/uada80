# Changelog

All notable changes to the uada80 project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project structure
- Project documentation:
  - ARCHITECTURE.md - Compiler architecture design
  - OPTIMIZATION_ANALYSIS.md - Analysis of reusable optimization code from uplm80
  - LANGUAGE_SUBSET.md - Ada language subset implementation plan
  - README.md - Project overview and getting started guide
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
- None

### Deprecated
- None

### Removed
- None

### Fixed
- None

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

### Phase 1: MVP (Target: 2-4 weeks)
- [ ] Lexer implementation
- [ ] Parser implementation (basic subset)
- [ ] AST node definitions
- [ ] Basic semantic analysis
- [ ] Simple code generator
- [ ] Compile "Hello World"

### Phase 2: Expanded Features (Target: 2-3 months)
- [ ] Full semantic analysis
- [ ] Package support
- [ ] Access types
- [ ] Enumeration types
- [ ] Port optimization from uplm80

### Phase 3: ACATS Compliance (Target: 6-12 months)
- [ ] Generic support
- [ ] Exception handling
- [ ] Full attribute support
- [ ] Pass core ACATS tests
- [ ] Performance tuning
