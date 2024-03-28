
CREATE TABLE IF NOT EXISTS test (
    c_integer INTEGER NOT NULL,
    c_varchar VARCHAR(1024) NOT NULL,
    c_date DATE NOT NULL,
    c_datetime DATETIME(6) NOT NULL,
    c_time TIME(6) NOT NULL,
    c_timestamp TIMESTAMP(6) NOT NULL
);
INSERT INTO test(c_integer, c_varchar, c_date, c_datetime, c_time, c_timestamp)
VALUES (123, 'str', '2019-02-13', '2019-02-13 22:03:21.051', '22:03:21.051', '2019-02-13 22:03:21.051');
