const Koa = require('koa')
const KoaRouter = require('koa-router')
const accessLog = require('koa-accesslog')
const proxy = require('koa-proxy')
const static = require('koa-static')

const BOT_SERVICE_HOST = 'http://localhost:4081'

const app = new Koa()
const router = new KoaRouter()

app.use(accessLog())
app.use(async (ctx, next) => {
  ctx.set('Access-Control-Allow-Origin', '*')
  ctx.set('Access-Control-Allow-Methods', 'OPTIONS, GET, PUT, POST, DELETE')
  ctx.set('Access-Control-Allow-Headers', 'Access-Control-Allow-Headers, Access-Control-Allow-Method, Access-Control-Allow-Origin, hedvig.token')

  await next()
})

app.use(async (ctx, next) => {
  if (ctx.request.method === 'OPTIONS') {
    ctx.status = 204
    return
  }

  await next()
})

app.use(static('./public'))

app.use(proxy({
  host: BOT_SERVICE_HOST,
  map: path => path.replace(/^\/bot-service/, ''),
  match: /^\/bot-service/,
}))
app.use(router.middleware())

let port = process.env.PORT || 3000
app.listen(port, () => {
  console.log('Listening on port ' + port)
})
