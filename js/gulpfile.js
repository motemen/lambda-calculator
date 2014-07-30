var gulp        = require('gulp'),
    $           = require('gulp-load-plugins')(),
    spawn       = require('child_process').spawn,
    browserSync = require('browser-sync');

function sbt(args, done) {
    var sbt = spawn('sbt', args, { cwd: __dirname + '/..', stdio: 'inherit' });
    sbt.on('close', function (exitCode) {
        if (exitCode !== 0) {
            throw "sbt exited with code " + exitCode;
        }
        if (done) done();
    });
}

var paths = {
    js: {
        fast: 'target/scala-2.11/js-fastopt.js',
        full: 'target/scala-2.11/js-opt.js'
    },
    html: 'src/main/html/call-by-value-untyped.html'
}

function buildHTML(js, dir) {
    var js = gulp.src(js).pipe(gulp.dest(dir + '/js'));

    return gulp.src(paths.html)
        .pipe($.inject(js, { addRootSlash: false, ignorePath: dir }))
        .pipe(gulp.dest(dir));
}

gulp.task('html', function () {
    return buildHTML(paths.js.fast, '.tmp');
});

gulp.task('serve', [ 'html' ], function (done) {
    browserSync({ server: { baseDir: '.tmp' } });

    sbt([ 'project js', '~fastOptJS' ]);
    gulp.watch('target/scala-2.11/js-fastopt.js', { debounceDelay: 2000 }, [ 'html' ]);
    gulp.watch('.tmp/**', browserSync.reload);
});

gulp.task('stage', [ 'sbt:fullOptJS' ], function () {
    return buildHTML(paths.js.full, 'publish');
});

gulp.task('sbt:fullOptJS', function (done) {
    sbt(['js/fullOptJS'], done);
});
