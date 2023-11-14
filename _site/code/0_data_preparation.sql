-- Original queries that were used to extract the data used for this analysis. The extracted datasets are stored in the eurodeer database and also stored in the data directory as rds files for roe and red 

-- ROE DEER 
DROP TABLE ws_fem.joh_clctcd_roe; 
CREATE TABLE ws_fem.joh_clctcd_roe AS(
  SELECT * FROM (
    SELECT row_number() over (partition by animals_id, datex, mid_or_noon, yearx) row_number, *, (animals_id || '_' || yearx) aniyr,
    min(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)) - min(doyx) over (partition by (animals_id || '_' || yearx)) diff_doy  
    FROM
    (-- fixes with requirement
      SELECT study_areas_id, sex, a.animals_id, acquisition_time, geom, forest_density, corine_land_cover_2012_vector_code,
      acquisition_time::time timex, TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                                                 / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date datex, extract(year from acquisition_time) yearx,
      CASE WHEN
      (acquisition_time BETWEEN
        ( (acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
        ( (acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes')) THEN 'noon' ELSE 'midnight' end mid_or_noon,
      extract(doy from acquisition_time) doyx
      FROM main.gps_data_animals a JOIN main.animals USING (animals_id)
      WHERE extract(month from acquisition_time) BETWEEN 5 and 10 AND gps_validity_code = 1
      AND
      (
        ( acquisition_time BETWEEN
          ((TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                         / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
          ((TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                         / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
        OR
        ( acquisition_time BETWEEN
          ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
          ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
      ) --AND animals_id = 1429 ORDER BY acquisition_time
    ) y --where animals_id = 1429 ORDER BY aniyr, acquisition_time
  ) z where row_number = 1 ORDER BY aniyr, acquisition_time
);




#CREATE TABLE ws_fem.joh_clctcd_roe AS(
#SELECT * FROM (
#SELECT row_number() over (partition by animals_id, datex, mid_or_noon, yearx) row_number, *, (animals_id || '_' || yearx) aniyr,
#min(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)) - min(doyx) over (partition by (animals_id || '_' || yearx)) diff_doy  
#FROM
#(-- fixes with requirement
#    SELECT study_areas_id, sex, a.animals_id, acquisition_time, geom, forest_density, corine_land_cover_2012_vector_code,
#    acquisition_time::time timex, acquisition_time::date datex, extract(year from acquisition_time) yearx,
#    CASE WHEN
#    (acquisition_time BETWEEN
#    ( (acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#    ( (acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes')) THEN 'midnight' ELSE 'noon' end mid_or_noon,
#    extract(doy from acquisition_time) doyx
#    FROM main.gps_data_animals a JOIN main.animals USING (animals_id)
#    WHERE extract(month from acquisition_time) BETWEEN 5 and 10 AND gps_validity_code = 1
#    AND
#    (
#    ( acquisition_time BETWEEN
#     ((acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#     ((acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
#    OR
#    ( acquisition_time BETWEEN
#    ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#   ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
#    )
#) y --where animals_id = 767
#) z where row_number = 1 ORDER BY aniyr, acquisition_time
#);


DROP TABLE ws_fem.joh_clctcd_roe_aniyr; 
CREATE TABLE ws_fem.joh_clctcd_roe_aniyr AS(
  -- PROPORTION OF FIXES TOTAL - DAY - NIGHT
  -- proportion of fixes
  SELECT total.study_areas_id, total.animals_id, total.aniyr, total.sex, count, prop, cnt_night, prop_night, cnt_day, prop_day FROM
  (
    SELECT count(*), count(*)/((max(diff_doy)+1)*2) prop, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem.joh_clctcd_roe
    WHERE diff_doy >= 183 AND study_areas_id in (1,2,8,15,25)
    GROUP BY animals_id, study_areas_id, sex, aniyr
    ORDER BY study_areas_id, animals_id
  ) total
  JOIN
  (
    -- proportion of fixes - night
    SELECT count(*) cnt_night, count(*)/(max(diff_doy)+1) prop_night, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem.joh_clctcd_roe
    WHERE diff_doy >= 183 and mid_or_noon = 'midnight' AND study_areas_id in (1,2,8,15,25)  
    GROUP BY animals_id, study_areas_id, sex, aniyr
    HAVING count(*)/(max(diff_doy)) > 0.50
    ORDER BY study_areas_id, animals_id
  ) night
  USING (aniyr)
  JOIN
  (
    -- proportion of fixes - day
    SELECT count(*) cnt_day, count(*)/(max(diff_doy)+1) prop_day, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem.joh_clctcd_roe
    WHERE diff_doy >= 183 and mid_or_noon = 'noon' AND study_areas_id in (1,2,8,15,25)
    GROUP BY animals_id, study_areas_id, sex, aniyr
    HAVING count(*)/(max(diff_doy)) > 0.50
    ORDER BY study_areas_id, animals_id
  ) day USING (aniyr)
);

-- animals and aniyrs
SELECT *, sum(cnt_animals) over () sum_animals, sum(cnt_aniyr) over () sum_aniyr
FROM
(
  --animals
  SELECT count(*) cnt_animals, a.study_areas_id FROM (SELECT DISTINCT animals_id, study_areas_id FROM ws_fem.joh_clctcd_roe_aniyr) a GROUP BY study_areas_id -- animals
) a
join
(
  --aniyrs
  SELECT count(*) cnt_aniyr, study_areas_id FROM ws_fem.joh_clctcd_roe_aniyr GROUP BY study_areas_id -- aniyrs
) b using (study_areas_id);

-- check aniyrs
SELECT * FROM ws_fem.joh_clctcd_roe_aniyr;





--------------------------------- RED DEER ---------------------------------
DROP TABLE ws_fem_reddeer.joh_clctcd_roe; 
CREATE TABLE ws_fem_reddeer.joh_clctcd_roe AS(
  SELECT * FROM (
    SELECT row_number() over (partition by animals_id, datex, mid_or_noon, yearx) row_number, *, (animals_id || '_' || yearx) aniyr,
    min(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)) - min(doyx) over (partition by (animals_id || '_' || yearx)) diff_doy  
    FROM
    (-- fixes with requirement
      SELECT study_areas_id, sex, a.animals_id, acquisition_time, geom, forest_density, corine_land_cover_2012_vector_code,
      acquisition_time::time timex, TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                                                 / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date datex, extract(year from acquisition_time) yearx,
      CASE WHEN
      (acquisition_time BETWEEN
        ( (acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
        ( (acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes')) THEN 'noon' ELSE 'midnight' end mid_or_noon,
      extract(doy from acquisition_time) doyx
      FROM main.gps_data_animals a JOIN main.animals USING (animals_id)
      WHERE extract(month from acquisition_time) BETWEEN 5 and 10 AND gps_validity_code = 1
      AND
      (
        ( acquisition_time BETWEEN
          ((TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                         / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
          ((TO_TIMESTAMP((EXTRACT(epoch FROM acquisition_time)::INTEGER + EXTRACT(epoch FROM interval '3 hour')::INTEGER / 2)
                         / EXTRACT(epoch FROM interval '3 hour')::INTEGER * EXTRACT(epoch FROM interval '3 hour')::INTEGER)::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
        OR
        ( acquisition_time BETWEEN
          ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
          ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
      ) --AND animals_id = 1429 ORDER BY acquisition_time
    ) y --where animals_id = 1429 ORDER BY aniyr, acquisition_time
  ) z where row_number = 1 ORDER BY aniyr, acquisition_time
);



#CREATE TABLE ws_fem_reddeer.joh_clctcd_red AS(
#SELECT * FROM (
#SELECT row_number() over (partition by animals_id, datex, mid_or_noon, yearx) row_number, *, (animals_id || '_' || yearx) aniyr,
#min(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)), max(doyx) over (partition by (animals_id || '_' || yearx)) - min(doyx) over (partition by (animals_id || '_' || yearx)) diff_doy  
#FROM
#(-- fixes with requirement
#    SELECT study_areas_id, sex, a.animals_id, acquisition_time, geom, forest_density, corine_land_cover_2012_vector_code,
#    acquisition_time::time timex, acquisition_time::date datex, extract(year from acquisition_time) yearx,
#    CASE WHEN
#    (acquisition_time BETWEEN
#    ( (acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#    ( (acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes')) THEN 'midnight' ELSE 'noon' end mid_or_noon,
#    extract(doy from acquisition_time) doyx
#    FROM main.gps_data_animals a JOIN main.animals USING (animals_id)
#    WHERE extract(month from acquisition_time) BETWEEN 5 and 10 AND gps_validity_code = 1
#    AND
#    (
#    ( acquisition_time BETWEEN
#     ((acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#     ((acquisition_time::date || ' ' || '00:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
#    OR
#    ( acquisition_time BETWEEN
#    ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone - interval '1 hour 30 minutes') and
#    ((acquisition_time::date || ' ' || '12:00:00'::time || '+00')::timestamp with time zone + interval '1 hour 30 minutes'))
#   )
#) y --where animals_id = 767
#) z where row_number = 1 ORDER BY aniyr, acquisition_time
#);


DROP TABLE ws_fem_reddeer.joh_clctcd_red_aniyr;
CREATE TABLE ws_fem_reddeer.joh_clctcd_red_aniyr AS(
  -- PROPORTION OF FIXES TOTAL - DAY - NIGHT
  -- proportion of fixes
  SELECT total.study_areas_id, total.animals_id, total.aniyr, total.sex, count, prop, cnt_night, prop_night, cnt_day, prop_day FROM
  (
    SELECT count(*), count(*)/((max(diff_doy)+1)*2) prop, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem_reddeer.joh_clctcd_red
    WHERE diff_doy >= 183 AND study_areas_id in (3,8,14,17,21)
    GROUP BY animals_id, study_areas_id, sex, aniyr
    ORDER BY study_areas_id, animals_id
  ) total
  JOIN
  (
    -- proportion of fixes - night
    SELECT count(*) cnt_night, count(*)/(max(diff_doy)+1) prop_night, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem_reddeer.joh_clctcd_red
    WHERE diff_doy >= 183 and mid_or_noon = 'midnight' AND study_areas_id in (3,8,14,17,21)
    GROUP BY animals_id, study_areas_id, sex, aniyr
    HAVING count(*)/(max(diff_doy)) > 0.50
    ORDER BY study_areas_id, animals_id
  ) night
  USING (aniyr)
  JOIN
  (
    -- proportion of fixes - day
    SELECT count(*) cnt_day, count(*)/(max(diff_doy)+1) prop_day, animals_id, aniyr, study_areas_id, sex
    FROM ws_fem_reddeer.joh_clctcd_red
    WHERE diff_doy >= 183 and mid_or_noon = 'noon' AND study_areas_id in (3,8,14,17,21)
    GROUP BY animals_id, study_areas_id, sex, aniyr
    HAVING count(*)/(max(diff_doy)) > 0.50
    ORDER BY study_areas_id, animals_id
  ) day USING (aniyr)
);

-- animals and aniyrs
SELECT *, sum(cnt_animals) over () sum_animals, sum(cnt_aniyr) over () sum_aniyr
FROM
(
  --animals
  SELECT count(*) cnt_animals, a.study_areas_id FROM (SELECT DISTINCT animals_id, study_areas_id FROM ws_fem_reddeer.joh_clctcd_red_aniyr) a GROUP BY study_areas_id -- animals
) a
join
(
  --aniyrs
  SELECT count(*) cnt_aniyr, study_areas_id FROM ws_fem_reddeer.joh_clctcd_red_aniyr GROUP BY study_areas_id -- aniyrs
) b using (study_areas_id);


-- check aniyrs
SELECT * FROM ws_fem_reddeer.joh_clctcd_red_aniyr;