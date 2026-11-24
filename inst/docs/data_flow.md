# Data Flow — Application Infrastructure, Security & Auditing (SSSR)

This document describes the end‑to‑end data flow, schemas, ERDs, and identity resolution pipelines that support universal application configurations, user directories, permissions, telemetry logging, and security auditing under the `01_SSSR` schema.

## High-Level Flow
1. **Request Lifecycle:** Active session headers initialized via Posit Connect, ShinyProxy, or local development environments.
2. **Identity Verification & JIT Provisioning:** The identity resolution engine maps user string tokens or network aliases to unique internal user IDs.
3. **Authorization & RBAC/ABAC Checks:** The active session resolves user privileges using a granular application-permission matrix.
4. **Active Application Operations:** The user interacts with business portals (e.g., Significant Change Portal, SLIC, Hubs Portal).
5. **Universal Log Generation:** Applications stream active navigation traces to clickstream telemetry and record database alterations to unified audit ledgers.
6. **Telemetry Analytics Reporting:** Security dashboards and operational trackers ingest the logs to evaluate system utilization and ensure strict compliance.

---

## System Architecture (Mermaid - view on GitHub)

```mermaid
flowchart TD
    A[User Client Session<br>HTTP Headers / Local Dev] -->|raw_token| B[db_user_create Helper]
    B -->|Cascade 1: Fast Match| C{users Registry}
    B -->|Cascade 2: Slower Match| D[user_identity_aliases]
    D -->|Translate to canonical email| C
    B -->|Cascade 3: Provision| E[JIT Create User Row]
    E --> C
    
    C -->|Return user_id INT| F[db_record_login Helper]
    F -->|Log telemetry event| G[app_analytics Table]
    
    H[Shiny Apps / Portals] -->|Check access| I[user_role_assignments]
    I -->|Lookup system role| J[roles_config]
    J -->|Resolve allowed actions| K[role_permissions / permissions_config]
    
    H -->|Transactional Action| L[db_write_audit_log]
    L -->|Strict auditing tracking| M[audit_logs Table]
    
    G & M --> N[Security & Platform Analytics Dashboards]
```

Diagrams
SSSR Core Architecture ERD (Mermaid - view on GitHub)
```mermaid
erDiagram
    users {
        INT user_id PK
        NVARCHAR username "NULL"
        NVARCHAR email "UK, NOT NULL"
        DATETIME2 created_at "NOT NULL"
    }

    user_identity_aliases {
        NVARCHAR username_alias PK
        NVARCHAR email_address "NOT NULL"
        DATETIME2 created_at "NOT NULL"
    }

    roles_config {
        INT role_id PK
        NVARCHAR role_name "UK, NOT NULL"
        NVARCHAR friendly_name "NOT NULL"
        NVARCHAR description "NULL"
    }

    permissions_config {
        INT permission_id PK
        NVARCHAR permission_name "UK, NOT NULL"
        NVARCHAR friendly_name "NOT NULL"
        NVARCHAR description "NULL"
    }

    role_permissions {
        INT role_id PK, FK
        INT permission_id PK, FK
    }

    user_role_assignments {
        INT assignment_id PK
        INT user_id FK, "NOT NULL"
        INT role_id FK, "NOT NULL"
        INT app_id FK, "NOT NULL"
        BIT is_active "NOT NULL"
        DATETIME2 assigned_at "NOT NULL"
        INT assigned_by FK, "NOT NULL"
        DATETIME2 revoked_at "NULL"
        INT revoked_by FK, "NULL"
    }

    apps_config {
        INT app_id PK
        NVARCHAR app_name "NOT NULL"
        NVARCHAR app_description "NULL"
        NVARCHAR app_url "NULL"
        NVARCHAR linking_table "NULL"
        NVARCHAR linking_column "NULL"
        BIT is_active "NOT NULL"
    }

    environments_config {
        TINYINT env_id PK
        NVARCHAR env_name "UK, NOT NULL"
        NVARCHAR friendly_name "NOT NULL"
    }

    app_analytics {
        INT analytics_id PK
        INT user_id FK, "NOT NULL"
        INT app_id FK, "NOT NULL"
        TINYINT env_id FK, "NOT NULL"
        DATETIME2 event_timestamp "NOT NULL"
        NVARCHAR page_name "NOT NULL"
        NVARCHAR action_type "NOT NULL"
        NVARCHAR action_sub_type "NULL"
    }

    audit_logs {
        BIGINT audit_id PK
        INT app_id FK, "NOT NULL"
        INT user_id FK, "NOT NULL"
        TINYINT env_id FK, "NOT NULL"
        NVARCHAR action_type "NOT NULL"
        NVARCHAR target_table "NOT NULL"
        NVARCHAR record_id "NULL"
        NVARCHAR action_summary "NOT NULL"
        DATETIME2 created_date "NOT NULL"
    }

    users ||--o{ user_role_assignments : "user_id"
    users ||--o{ user_role_assignments : "assigned_by"
    users ||--o{ user_role_assignments : "revoked_by"
    roles_config ||--o{ user_role_assignments : "role_id"
    apps_config ||--o{ user_role_assignments : "app_id"
    
    roles_config ||--o{ role_permissions : "role_id"
    permissions_config ||--o{ role_permissions : "permission_id"
    
    users ||--o{ app_analytics : "user_id"
    apps_config ||--o{ app_analytics : "app_id"
    environments_config ||--o{ app_analytics : "env_id"
    
    users ||--o{ audit_logs : "user_id"
    apps_config ||--o{ audit_logs : "app_id"
    environments_config ||--o{ audit_logs : "env_id"
```
Source Data
Core Platform Config & Security Schema — 01_SSSR
[01_SSSR].[users]: Master corporate user registry mapping canonical accounts to a unique tracking integer identifier.

[01_SSSR].[user_identity_aliases]: Lookup index mapping diverse single sign-on or environment token strings to their verified email addresses.

[01_SSSR].[roles_config]: Reference catalog tracking active access tiers across the platform ecosystem.

[01_SSSR].[permissions_config]: Fine-grained authorization tokens mapped to distinct interface features.

[01_SSSR].[role_permissions]: Relationship junction connecting configuration rules to designated platform roles.

[01_SSSR].[user_role_assignments]: Application permissions matrix verifying active access scopes. Features soft-delete audit hooks.

[01_SSSR].[apps_config]: Master inventory tracking deployed applications, routing addresses, and connection targets.

[01_SSSR].[environments_config]: Isolated identifiers segmenting Local Development, Test/QA, and Production run environments.

[01_SSSR].[app_analytics]: Standardized tracking database capturing internal navigation actions and user event triggers.

[01_SSSR].[audit_logs]: High-fidelity transactional database audit ledger detailing structural table mutations.

Technical Workflows
Identity Resolution & Just-In-Time Onboarding Pipeline
Purpose: Convert raw username or email string tokens safely into normalized user_id integer primary keys at session startup before rendering active UI.

Run Cadence: Executed dynamically inside Shiny server initialization hooks via dauPortalTools::db_user_create().

Process Flow:

```mermaid
flowchart LR
    A[Capture Session Token] --> B{Exact Match in Users?}
    B -- Yes (Fast Path) --> C[Return user_id]
    B -- No --> D{Exists in Aliases?}
    D -- Yes (Slower Path) --> E[Map to Canonical Email] --> B
    D -- No (Slowest Path) --> F[JIT Provision New User] --> G[Write user row] --> C
```
D -- No (Slowest Path) --> F[JIT Provision New User] --> G[Write user row] --> C
Extract: Intercept token string inputs via active directory connection headers or local workspace environment properties.

Fast-Path Check: Evaluate if the input directly matches an existing record inside [01_SSSR].[users]. If a match occurs, return the user_id.

Alias Evaluation: Fall back to [01_SSSR].[user_identity_aliases] if unmatched to resolve corporate shorthands to target email accounts.

Provision: If completely new, execute an automated insert into [01_SSSR].[users], setting default parameters and returning the identity key.

Transactional Auditing Framework
Purpose: Record all row insertions, balance changes, or structural record mutations across production datasets to maintain a complete historical trace.

Process Flow:


```mermaid
flowchart LR
    A[Transactional Mutation] --> B[db_write_audit_log R function]
    B --> C[Resolve App ID & Active Env]
    C --> D[Insert event detail to 01_SSSR.audit_logs]
    D --> E[Write Success Status to R Console]
```
Trigger: A database mutation query executes inside an active user dashboard session.

Context Enrichment: The package evaluates the context to determine the calling app_id and execution env_id (e.g., Development vs. Production).

Log Execution: The application executes a structured insert to [01_SSSR].[audit_logs], recording the modified table, targeted record primary key, execution type, and details of the operation.

Outputs
Unified Access Security Logs: Comprehensive system review sheets used to audit user permissions across the environment.

Usage Performance Dashboards: Clickstream analytics metrics reporting active user volume trends.

Access Matrix Control Blocks: Administrative control panels used to update, add, or soft-delete assignments dynamically without altering code.

---

## Platform Catalogue, Messaging & Governance Schema (`[01_SSSR]`)

In addition to core identity, RBAC, telemetry, and audit tables (`users`, `user_identity_aliases`, `roles_config`, `permissions_config`, `role_permissions`, `user_role_assignments`, `apps_config`, `environments_config`, `app_analytics`, `audit_logs`), `[01_SSSR]` hosts shared service messaging and data-governance metadata tables used across SLIC and DAU portals:

### Catalogue & Messaging ERD

```mermaid
erDiagram
    users ||--o{ portal_messages : "authored by (user_id)"
    apps_config ||--o{ portal_messages : "scoped to (app_id)"

    app_list ||--o{ app_maintainers : "maintained by (app_id)"
    users ||--o{ app_maintainers : "maintains (user_id)"

    data_type ||--o{ data_table : "classifies (data_type_fk)"
    data_table ||--o{ data_table_owners : "owned by (data_id)"
    users ||--o{ data_table_owners : "owns (user_id)"

    app_list ||--o{ app_data_links : "consumes (app_id)"
    data_table ||--o{ app_data_links : "linked via (data_id)"

    portal_messages {
        INT message_id PK
        DATETIME2 message_date
        NVARCHAR message_text
        INT app_id FK
        TINYINT priority
        DATETIME2 created_on
        BIT is_active
        INT user_id FK
    }

    app_list {
        INT app_id PK
        NVARCHAR app_name
        VARCHAR app_description
        VARCHAR app_url
        BIT app_active
        NVARCHAR version
        NVARCHAR team
        NVARCHAR update_cycle
        DATETIME2 created_date
        INT created_by
        DATETIME2 modified_date
        INT modified_by
        DATE reviewed_date
        INT reviewed_by
        NVARCHAR app_guidance
    }

    app_maintainers {
        INT id PK
        INT app_id FK
        INT user_id FK
        BIT is_primary
        DATETIME2 created_date
        INT created_by
    }

    data_type {
        INT type_id PK
        NVARCHAR type_name
        NVARCHAR type_description
        BIT is_active
        VARCHAR type_colour
    }

    data_table {
        INT data_id PK
        INT data_type_fk FK
        NVARCHAR data_name
        NVARCHAR data_table
        NVARCHAR data_pk
        VARCHAR platform
        VARCHAR source_scope
        NVARCHAR data_description
        BIT is_active
    }

    data_table_owners {
        INT owner_link_id PK
        INT data_id FK
        INT user_id FK
        BIT is_primary
    }

    app_data_links {
        INT link_id PK
        INT app_id FK
        INT data_id FK
        VARCHAR access_mode
        BIT is_active
    }
```

### Table Reference

* **`[01_SSSR].[portal_messages]`**: Service announcements and operational banners queried via `db_get_portal_messages()`. Supports global (`app_id = 1`) and app-specific scoping, priority ordering (`1` = urgent/alert, `2` = standard), and soft deletion (`is_active`).
* **`[01_SSSR].[app_list]`**: Metadata catalogue of DAU applications (`version`, `team`, `update_cycle`, `reviewed_date`, `app_guidance`) surfaced in SLIC (`dau-sssr-finally`) and portal `About` pages.
* **`[01_SSSR].[app_maintainers]`**: Maps primary and secondary technical maintainers (`user_id`, `is_primary`) to entries in `app_list`.
* **`[01_SSSR].[data_type]`**: Lookup defining dataset classifications (`type_name`, `type_colour` hex badge).
* **`[01_SSSR].[data_table]`**: Central data asset register recording physical SQL Server or Databricks tables (`platform` in `'SQL'`, `'DataBricks'`), governance tier (`source_scope` in `'DAU'`, `'Internal'`, `'External'`), primary key column (`data_pk`), and descriptions.
* **`[01_SSSR].[data_table_owners]`**: Maps canonical `users` to owned `data_table` records (`is_primary`).
* **`[01_SSSR].[app_data_links]`**: Many-to-many lineage bridge mapping `app_list` applications to `data_table` assets with explicit `access_mode` (`'READ'`, `'WRITE'`, `'READ_WRITE'`).

### R Function Mapping (`dauPortalTools`)

| Domain | R Source File(s) | Key Functions | Target Tables |
| :--- | :--- | :--- | :--- |
| **Identity & Session** | `R/db-user.R`, `R/utils-user_auth.R` | `utils_get_user()`, `utils_record_login()`, `db_update_user_last_login()` | `users`, `user_identity_aliases` |
| **RBAC & Permissions** | `R/db-user_roles.R`, `R/utils-permissions.R` | `utils_get_user_roles()`, `assign_user_role()`, `revoke_user_role()`, `user_has_permission()` | `roles_config`, `permissions_config`, `role_permissions`, `user_role_assignments` |
| **Telemetry & Audit** | `R/db-analytics_helpers.R`, `R/db-audit_helpers.R` | `log_page_view()`, `log_user_action()`, `log_transaction()`, `db_write_audit_log()` | `app_analytics`, `audit_logs`, `apps_config`, `environments_config` |
| **Portal Banners** | `R/db-message_helpers.R` | `db_get_portal_messages()`, `db_add_portal_message()`, `db_edit_portal_message()`, `db_deactivate_portal_message()` | `portal_messages` |
| **App & Data Catalogue** | `R/db-apps.R`, `R/db-admin_helpers.R` | `db_get_all_apps()`, `db_save_app()`, `db_get_app_maintainers()`, `db_add_app_maintainer()` | `app_list`, `app_maintainers`, `data_type`, `data_table`, `data_table_owners`, `app_data_links` |