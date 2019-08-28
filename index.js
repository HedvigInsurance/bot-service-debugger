const Koa = require('koa')
const KoaRouter = require('koa-router')
const accessLog = require('koa-accesslog')
const axios = require('axios').default

const app = new Koa()
const router = new KoaRouter()

app.use(accessLog())
app.use(async (ctx, next) => {
  ctx.set('Access-Control-Allow-Origin', '*')

  await next()
})

router.get('/proxy', async ctx => {
  ctx.body = (await axios.get('https://elm-lang.org/assets/public-opinion.txt')).data
})

app.use(router.middleware())

let port = process.env.PORT || 3000
app.listen(port, () => {
  console.log('Listening on port ' + port)
})
