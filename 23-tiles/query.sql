# here is the BigQuery standardSql query used to generate the data for this chart
select year, t, count(*) as count from(
  SELECT
  extract(year from date) as year,
  t
  FROM (
    WITH
    date_tags AS (
      SELECT
      date,
      SPLIT(tags, "|") AS t
      FROM (
        SELECT
        DATE(creation_date) AS date,
        tags
        FROM
        `bigquery-public-data.stackoverflow.stackoverflow_posts` ))
    SELECT
    date,
    t
    FROM
    date_tags
    CROSS JOIN
    UNNEST(date_tags.t) AS tags),
  UNNEST(t) t)
group by year, t order by year desc, count desc