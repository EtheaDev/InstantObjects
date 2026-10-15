# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

InstantObjects is a RAD framework for Delphi that enables creation of applications based on persistent business objects. It provides an Object-Relational Mapping (ORM) layer supporting multiple database backends and includes tools for model-driven development.

**Current Version:** 4.3.2 (supports Delphi XE6 through Delphi 13 Florence)

## Build and Test Commands

### Building Packages

Packages are organized by Delphi version in `Source/PackageGroups/<version>/`:
- `1.RunTimePackages.groupproj` - Runtime packages
- `2.DesignTimePackages.groupproj` - Design-time packages

**Build order:**
1. Build runtime packages first (IOCore_4, then broker packages)
2. Install design-time packages (dclIOCore_4, then broker design packages)

**Common package naming:**
- Runtime: `IOCore_4`, `IOADO_4`, `IODBX_4`, `IOIBX_4`, `IOXML_4`, `IOFireDAC_4`, `IOJSON_4`
- Design: `dclIOCore_4`, `dclIOADO_4`, `dclIODBX_4`, `dclIOIBX_4`, `dclIOXML_4`, `dclIOFireDAC_4`, `dclIOJSON_4`
- Catalogs: `IOIBFbCatalog_4`, `IOMSSqlCatalog_4`, `IOMySQLCatalog_4` (and their dcl counterparts)

### Running Tests

InstantObjects uses DUnitX for unit testing (400+ tests):

**Test projects by Delphi version:**
- `Tests/TestIO_D10_2.dpr` through `Tests/TestIO_D13.dpr` - DUnitX tests
- `Tests/TestIO_DUnit_D11.dpr` through `Tests/TestIO_DUnit_D13.dpr` - DUnit tests

**Run tests:**
```
dcc32 Tests\TestIO_D13.dpr
Tests\TestIO_D13.exe
```

Compatible with TestInsight framework for IDE integration.

### Demo Applications

**PrimerCross** - Main cross-platform demo (located in `Demos/PrimerCross/`):
```
Primer_D13.dpr (for Delphi 13)
PrimerExternal.dpr (demonstrates external storage)
```

**Other demos:**
- `Intro` - Bare bones introduction
- `PrimerFireMonkey` - FireMonkey UI example
- `Pump` - InstantPump component usage
- `ConsoleApp` - Console application example
- `BuildPrimerDatabase` - Database creation example

## Architecture

### Core Persistence Layer

**Key units in `Source/Core/`:**

- `InstantPersistence.pas` - Core persistence classes: `TInstantObject`, `TInstantAttribute`, `TInstantConnector`
- `InstantBrokers.pas` - Broker architecture: `TInstantBroker`, `TInstantSQLBroker`, resolvers, translators
- `InstantMetadata.pas` - Metadata system for object-to-database mapping
- `InstantPresentation.pas` - Data-aware components: `TInstantExposer`, `TInstantSelector`
- `InstantClasses.pas` - Base class infrastructure
- `InstantTypes.pas` - Type definitions and enumerations

### Object Model

All persistent business objects inherit from `TInstantObject` which provides:
- Persistence lifecycle: `Store()`, `Retrieve()`, `Refresh()`, `Dispose()`
- Change tracking: `IsChanged`, `IsPersistent`
- Attribute management via typed attribute classes

**Attribute types:**
- Simple: `TInstantString`, `TInstantInteger`, `TInstantFloat`, `TInstantBoolean`, `TInstantDateTime`, `TInstantDate`, `TInstantTime`, `TInstantCurrency`
- Complex: `TInstantBlob`, `TInstantMemo`, `TInstantGraphic`
- Relationships: `TInstantReference` (1:1), `TInstantReferences` (1:many), `TInstantPart` (composition), `TInstantParts` (composition collection)

### Metadata Declaration

Classes use special IOMETADATA comments for code generation:

```pascal
TContact = class(TInstantObject)
{IOMETADATA stored 'CONTACTS';
  Name: String(50) stored 'CONTACT_NAME' index required;
  Email: String(100) stored 'EMAIL';
  Category: Reference(TCategory) stored 'CATEGORY_ID';
  Phones: Parts(TPhone) stored 'CONTACT_ID' external;}
  _Name: TInstantString;
  _Email: TInstantString;
  _Category: TInstantReference;
  _Phones: TInstantParts;
```

**Storage modes:**
- Internal (default) - Stored in same table
- External - Stored in separate table with foreign key

### Broker Architecture

InstantObjects supports two broker types:

**SQL-based brokers** (preferred for relational databases):
- Components: `TInstantSQLBroker`, `TInstantSQLResolver`, `TInstantSQLGenerator`, `TInstantRelationalTranslator`
- Features: External storage, optimized queries, statement caching
- Available for: ADO, DBX, IBX, UIB, FireDAC

**Dataset-based brokers**:
- Components: `TInstantNavigationalBroker`, `TInstantNavigationalResolver`
- Simpler architecture, limited features
- Available for: XML, JSON, BDE

### Broker-Catalog Dependencies

Some brokers require separate catalog packages:

| Broker | Required Catalog(s) |
|--------|-------------------|
| IBX | IBFb |
| UIB | IBFb |
| DBX | IBFb, MSSql, MySQL |
| ADO | MSSql |
| FireDAC | IBFb, MSSql, MySQL |

### Data Presentation

**TInstantExposer** - Exposes a single object's attributes as dataset fields
- Supports edit, insert, delete operations
- Options: `eoAutoApply`, `eoDeferInsert`, `eoAutoRemember`

**TInstantSelector** - Exposes collection of objects as dataset
- Supports filtering and sorting
- Integrated with standard data-aware controls
- Uses InstantIQL (InstantObjects Query Language)

### JSON/REST Support

**Serialization (requires delphi-neon library):**
- `Instant.Neon.Serializers.pas` - JSON serialization/deserialization
- `TInstantJSONConnector` - JSON file-based storage
- Enable with `{$DEFINE DELPHI_NEON}` in `InstantDefines.inc`

**REST Server support:**
- MARS Curiosity - `Source/MARSServer/` units, demo: `Demos/PrimerRESTServer/`
- WiRL - `Source/WiRLServer/` units, demo: `Demos/PrimerWiRLServer/`

## Important Compiler Directives

Located in `Source/InstantDefines.inc`:

- `{$DEFINE USE_LARGEINT_FIELD_FOR_REF}` - Use TLargeIntField for references (recommended for 64-bit)
- `{$DEFINE DELPHI_NEON}` - Enable JSON serialization support
- `{$DEFINE IO_DISABLE_STRING_TRIM}` - Disable automatic right-trim of strings
- `{$DEFINE IO_STATEMENT_LOGGING}` - Log SQL statements to debug output
- `{$DEFINE IO_NO_CIRCULAR_REFERENCE_CHECK}` - Disable circular reference checking (use only if certain no circular refs exist)

## Database Operations

### Building Databases

At design-time: Use IDE menu "View > Build InstantObjects Database..." (requires ConnectionManager setup)

At runtime: Use `TInstantDBBuilder` or `TInstantDBEvolution` classes

### Database Evolution

InstantObjects supports schema evolution through:
- `InstantDBEvolution.pas` - Database structure evolution
- Can add/remove/modify tables and columns
- Preserves existing data during evolution

### Connection Management

`TInstantConnectionManager` provides:
- Design-time connection definition editor
- Multiple connection profiles
- Connection file formats: `.con` (binary) or `.xml`

## Model Development

### Model Explorer

Design-time tool for visual model editing:
- Class hierarchy view
- Relationship visualization
- Attribute editor
- Two-way code generation

### ObjectFoundry Integration

Supports ModelMaker integration for UML-driven development (ModelMaker 6.20-9.x).

## Testing Infrastructure

**ubMock framework** (included in `Tests/ubmock/`):
- Unit test mocking framework
- Used for all InstantObjects unit tests

**Test organization:**
- Individual test units for each component (`TestInstant*.pas`)
- `TestFullModel.pas` - Comprehensive integration tests
- `TestMockBroker.pas`, `TestXMLBroker.pas` - Broker-specific tests
- `MinimalModel.pas`, `TestModel.pas` - Test model classes

## Unicode and 64-bit Support

- Full Unicode support controlled by `TInstantConnection.UseUnicode` flag
- 64-bit Windows platform support (define `WINLINUX64` for 64-bit builds)
- Use `TLargeIntField` for object IDs in 64-bit applications

## Memory Management

- Reference counting for object relationships
- Automatic circular reference detection and cleanup (unless disabled)
- `TInstantObjectStore` manages object cache
- Ownership semantics differ between Parts (owned) and References (not owned)

## Package Structure by Delphi Version

Supported versions: D10_1, D10_2, D10_3, D10_4, D11, D12, D13

Each broker and component has version-specific packages in subdirectories.
