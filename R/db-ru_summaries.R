#' Retrieve RISE Universal Hubs Regional Summary by GOR
#'
#' Executes an entity-level regional aggregation across the 9 English GORs
#' based on active search views and pre-aggregated transactional tables.
#'
#' @param con A DBI database connection.
#' @param schema Character target RISE schema (e.g. "[01_RISE]" or "[01_RISE_b]").
#' @return A data.frame containing regional aggregates.
#' @export
db_ru_get_regional_summary <- function(
    schema = utils_resolve_schema("db_schema_01r")
) {
    conn <- sql_manager("dit")

    query <- glue::glue(
        "
    DECLARE @RollingWindowStart DATE = DATEADD(YEAR, -1, CAST(GETDATE() AS DATE));

WITH 
-- 1. Combine and deduplicate active transactional entities into a tiny working set (~300 rows)
ActiveTransactions AS (
    -- Events
    SELECT 
        ruev_entity_type AS entity_type,
        LTRIM(RTRIM(
            CASE 
                WHEN CHARINDEX('.', CAST(ruev_entity_id AS NVARCHAR(50))) > 0 
                THEN LEFT(CAST(ruev_entity_id AS NVARCHAR(50)), CHARINDEX('.', CAST(ruev_entity_id AS NVARCHAR(50))) - 1)
                ELSE CAST(ruev_entity_id AS NVARCHAR(50))
            END
        )) AS clean_entity_id,
        ruev_id AS event_id,
        NULL AS lead_id,
        NULL AS hub_id,
        NULL AS is_supported
    FROM [01_RISE].[ru_events]
    WHERE (ruev_completed = 0 OR (ruev_completed = 1 AND ruev_date >= @RollingWindowStart))
      AND ruev_entity_type IN ('School', 'Trust', 'LA')

    UNION ALL

    -- Lead Schools
    SELECT 
        ISNULL(ruhl_entity_type, 'School') AS entity_type,
        LTRIM(RTRIM(
            CASE 
                WHEN CHARINDEX('.', CAST(ruhl_entity_id AS NVARCHAR(50))) > 0 
                THEN LEFT(CAST(ruhl_entity_id AS NVARCHAR(50)), CHARINDEX('.', CAST(ruhl_entity_id AS NVARCHAR(50))) - 1)
                ELSE CAST(ruhl_entity_id AS NVARCHAR(50))
            END
        )) AS clean_entity_id,
        NULL AS event_id,
        ruhl_id AS lead_id,
        NULL AS hub_id,
        NULL AS is_supported
    FROM [01_RISE].[ru_lead_schools]
    WHERE ruhl_active = 1
      AND ISNULL(ruhl_entity_type, 'School') IN ('School', 'Trust', 'LA')

    UNION ALL

    -- Support Records
    SELECT 
        ruhsr_entity_type AS entity_type,
        LTRIM(RTRIM(
            CASE 
                WHEN CHARINDEX('.', CAST(ruhsr_entity_id AS NVARCHAR(50))) > 0 
                THEN LEFT(CAST(ruhsr_entity_id AS NVARCHAR(50)), CHARINDEX('.', CAST(ruhsr_entity_id AS NVARCHAR(50))) - 1)
                ELSE CAST(ruhsr_entity_id AS NVARCHAR(50))
            END
        )) AS clean_entity_id,
        NULL AS event_id,
        NULL AS lead_id,
        ruhb_id AS hub_id,
        1 AS is_supported
    FROM [01_RISE].[ruh_support_records]
    WHERE ruhsr_active = 1 
      AND ruhb_id > 0
      AND ruhsr_entity_type IN ('School', 'Trust', 'LA')
),

-- 2. Aggregate metrics at the unique entity level
EntityMetrics AS (
    SELECT 
        entity_type,
        clean_entity_id,
        TRY_CAST(clean_entity_id AS INT) AS clean_entity_id_int,
        COUNT(DISTINCT event_id) AS n_events,
        COUNT(DISTINCT lead_id) AS n_lead_schools,
        COUNT(DISTINCT hub_id) AS n_hubs_serving,
        MAX(is_supported) AS is_supported
    FROM ActiveTransactions
    GROUP BY entity_type, clean_entity_id
),

-- 3. Join the 3 search views ONCE against only ~300 rows
EntityWithRegion AS (
    SELECT 
        COALESCE(s.region, t.lead_region, la.region) AS gor_name,
        em.entity_type,
        em.clean_entity_id,
        em.n_events,
        em.n_lead_schools,
        em.n_hubs_serving,
        em.is_supported
    FROM EntityMetrics em
    LEFT JOIN [01_RISE].[vw_ru_search_schools] s 
        ON em.entity_type = 'School' AND em.clean_entity_id_int = s.urn
    LEFT JOIN [01_RISE].[vw_ru_search_trusts] t 
        ON em.entity_type = 'Trust' AND em.clean_entity_id = CAST(t.trust_id AS NVARCHAR(50))
    LEFT JOIN [01_RISE].[vw_ru_search_la] la 
        ON em.entity_type = 'LA' AND em.clean_entity_id = CAST(la.la_code AS NVARCHAR(50))
),

-- 4. Complete set of 9 GOR names
EnglishRegions AS (
    SELECT DISTINCT region AS gor_name 
    FROM [01_RISE].[vw_ru_search_schools]
    WHERE region IS NOT NULL 
      AND region NOT IN ('', 'Not Applicable', 'Wales (pseudo)')
)

-- 5. Final Regional Rollup
SELECT 
    r.gor_name,
    ISNULL(MAX(ewr.n_hubs_serving), 0) AS n_hubs,
    COUNT(DISTINCT CASE WHEN ewr.is_supported = 1 
                        THEN CONCAT(ewr.entity_type, '_', ewr.clean_entity_id) 
                   END) AS n_supported_entities,
    COUNT(DISTINCT CASE WHEN ewr.n_lead_schools > 0 
                        THEN CONCAT(ewr.entity_type, '_', ewr.clean_entity_id) 
                   END) AS n_lead_entities,
    ISNULL(SUM(ewr.n_events), 0) AS n_events,
    COUNT(DISTINCT CASE WHEN ewr.n_events > 0 
                        THEN CONCAT(ewr.entity_type, '_', ewr.clean_entity_id) 
                   END) AS n_event_entities
FROM EnglishRegions r
LEFT JOIN EntityWithRegion ewr 
    ON r.gor_name = ewr.gor_name
GROUP BY r.gor_name
ORDER BY r.gor_name;
  "
    )

    utils_db_get_query(conn, query)
}
