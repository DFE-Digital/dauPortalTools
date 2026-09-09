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
    -- 1. Base Region Map: Exclusively School, Trust, and LA entities
    EntityRegionMap AS (
        SELECT 'School' AS entity_type, CAST([urn] AS NVARCHAR(50)) AS entity_id, [region] AS gor_name
        FROM {schema}.[vw_ru_search_schools]
        WHERE [region] IS NOT NULL AND [region] NOT IN ('', 'Not Applicable', 'Wales (pseudo)')

        UNION ALL

        SELECT 'Trust' AS entity_type, CAST([trust_id] AS NVARCHAR(50)) AS entity_id, [lead_region] AS gor_name
        FROM {schema}.[vw_ru_search_trusts]
        WHERE [lead_region] IS NOT NULL AND [lead_region] NOT IN ('', 'Not Applicable', 'Wales (pseudo)')

        UNION ALL

        SELECT DISTINCT 'LA' AS entity_type, CAST([la_code] AS NVARCHAR(50)) AS entity_id, [region] AS gor_name
        FROM {schema}.[vw_ru_search_la]
        WHERE [region] IS NOT NULL AND [region] NOT IN ('', 'Not Applicable', 'Wales (pseudo)')
    ),

    -- 2. Pre-aggregate Events at Entity Level
    EventsPreAgg AS (
        SELECT 
            [ruev_entity_type] AS entity_type,
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruev_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruev_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruev_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruev_entity_id] AS NVARCHAR(50))
                END
            )) AS entity_id,
            COUNT(DISTINCT [ruev_id]) AS n_events
        FROM {schema}.[ru_events]
        WHERE ([ruev_completed] = 0 OR ([ruev_completed] = 1 AND [ruev_date] >= @RollingWindowStart))
          AND [ruev_entity_type] IN ('School', 'Trust', 'LA')
        GROUP BY 
            [ruev_entity_type],
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruev_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruev_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruev_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruev_entity_id] AS NVARCHAR(50))
                END
            ))
    ),

    -- 3. Pre-aggregate Lead Entities (Pointing to ru_lead_schools)
    LeadSchoolsPreAgg AS (
        SELECT 
            ISNULL([ruhl_entity_type], 'School') AS entity_type,
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruhl_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruhl_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruhl_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruhl_entity_id] AS NVARCHAR(50))
                END
            )) AS entity_id,
            COUNT(DISTINCT [ruhl_id]) AS n_lead_schools
        FROM {schema}.[ru_lead_schools]
        WHERE [ruhl_active] = 1
          AND ISNULL([ruhl_entity_type], 'School') IN ('School', 'Trust', 'LA')
        GROUP BY 
            ISNULL([ruhl_entity_type], 'School'),
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruhl_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruhl_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruhl_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruhl_entity_id] AS NVARCHAR(50))
                END
            ))
    ),

    -- 4. Pre-aggregate Hub Support Records at Entity Level
    SupportRecordsPreAgg AS (
        SELECT 
            [ruhsr_entity_type] AS entity_type,
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruhsr_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruhsr_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruhsr_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruhsr_entity_id] AS NVARCHAR(50))
                END
            )) AS entity_id,
            COUNT(DISTINCT [ruhb_id]) AS n_hubs_serving
        FROM {schema}.[ruh_support_records]
        WHERE [ruhsr_active] = 1 
          AND [ruhb_id] > 0
          AND [ruhsr_entity_type] IN ('School', 'Trust', 'LA')
        GROUP BY 
            [ruhsr_entity_type],
            LTRIM(RTRIM(
                CASE 
                    WHEN CHARINDEX('.', CAST([ruhsr_entity_id] AS NVARCHAR(50))) > 0 
                    THEN LEFT(CAST([ruhsr_entity_id] AS NVARCHAR(50)), CHARINDEX('.', CAST([ruhsr_entity_id] AS NVARCHAR(50))) - 1)
                    ELSE CAST([ruhsr_entity_id] AS NVARCHAR(50))
                END
            ))
    ),

    -- 5. Distinct 9 English GOR Names
    EnglishRegions AS (
        SELECT DISTINCT [region] AS gor_name 
        FROM {schema}.[vw_ru_search_schools]
        WHERE [region] IS NOT NULL 
          AND [region] NOT IN ('', 'Not Applicable', 'Wales (pseudo)')
    ),

    -- 6. Attach pre-aggregations directly to entities
    EntityTotals AS (
        SELECT 
            m.gor_name,
            m.entity_type,
            m.entity_id,
            ISNULL(ev.n_events, 0)       AS n_events,
            ISNULL(ls.n_lead_schools, 0) AS n_lead_schools,
            ISNULL(sr.n_hubs_serving, 0) AS n_hubs_serving
        FROM EntityRegionMap m
        LEFT JOIN EventsPreAgg ev 
            ON m.entity_type = ev.entity_type 
           AND m.entity_id = ev.entity_id
        LEFT JOIN LeadSchoolsPreAgg ls 
            ON m.entity_type = ls.entity_type 
           AND m.entity_id = ls.entity_id
        LEFT JOIN SupportRecordsPreAgg sr 
            ON m.entity_type = sr.entity_type 
           AND m.entity_id = sr.entity_id
        WHERE ev.n_events > 0 
           OR ls.n_lead_schools > 0 
           OR sr.n_hubs_serving > 0
    )

    -- 7. Pure Entity-Level Regional Rollup
    SELECT 
        r.gor_name,
        ISNULL(MAX(et.n_hubs_serving), 0) AS n_hubs,
        COUNT(DISTINCT CASE WHEN et.n_hubs_serving > 0 
                            THEN CONCAT(et.entity_type, '_', et.entity_id) 
                       END) AS n_supported_entities,
        COUNT(DISTINCT CASE WHEN et.n_lead_schools > 0 
                            THEN CONCAT(et.entity_type, '_', et.entity_id) 
                       END) AS n_lead_entities,
        ISNULL(SUM(et.n_events), 0) AS n_events,
        COUNT(DISTINCT CASE WHEN et.n_events > 0 
                            THEN CONCAT(et.entity_type, '_', et.entity_id) 
                       END) AS n_event_entities
    FROM EnglishRegions r
    LEFT JOIN EntityTotals et ON r.gor_name = et.gor_name
    GROUP BY r.gor_name
    ORDER BY r.gor_name;
  "
  )

  utils_db_get_query(conn, query)
}
