var gulp       = require('gulp'),
    $          = require('gulp-load-plugins')(),
    spawn      = require('child_process').spawn;

gulp.task('production', [ 'sbt:js' ], function () {
    var js = gulp.src('target/scala-2.11/js-opt.js')
                 .pipe(gulp.dest('publish/js'));

    gulp.src('src/main/html/call-by-value-untyped.html')
        .pipe($.inject(js, { addRootSlash: false, ignorePath: 'publish/' }))
        .pipe(gulp.dest('publish'));
});

gulp.task('sbt:js', function (done) {
    var sbt = spawn('sbt', [ 'js/fullOptJS' ], { cwd: __dirname + '/..', stdio: 'inherit' });
    sbt.on('close', function (exitCode) {
        if (exitCode !== 0) {
            throw "sbt exited with code " + exitCode;
        }
        done();
    });
});
