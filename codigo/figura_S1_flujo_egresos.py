# =============================================================================
# figura_S1_flujo_egresos.py -- Diagrama de flujo de egresos (Archivo adicional 2 del manuscrito)
# Dibuja el flujo de la base Sectorial 2014-2024 a la muestra analitica del escenario
# principal E6b. Las cifras provienen de la salida de flujo_participantes_y_riesgo_absoluto_E6b.R
# (hojas flujo y mortalidad_cruda); si cambia el analisis, actualizarlas aqui.
# Requiere: Python 3 y matplotlib. Salida: figS1_flow_en.jpeg (300 dpi).
# =============================================================================
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
fig,ax=plt.subplots(figsize=(9.2,10)); ax.set_xlim(0,100); ax.set_ylim(0,100); ax.axis("off")
NAVY="#1F3A5F"; GREY="#EEF1F5"; ACC="#2E6F9E"
def box(x,y,w,h,txt,fc=GREY,ec=NAVY,bold=False,fs=10):
    ax.add_patch(FancyBboxPatch((x,y),w,h,boxstyle="round,pad=0.3,rounding_size=1.2",fc=fc,ec=ec,lw=1.2))
    ax.text(x+w/2,y+h/2,txt,ha="center",va="center",fontsize=fs,color="#111",fontweight="bold" if bold else "normal",linespacing=1.25)
def arr(x1,y1,x2,y2): ax.annotate("",xy=(x2,y2),xytext=(x1,y1),arrowprops=dict(arrowstyle="-|>",color=NAVY,lw=1.3))
W=44; X=4
box(X,84,W,13,"Hospital discharges, Sectorial database\n2014–2024 (including in-hospital deaths)\nn = 61,588,637\nPotentially rare: 1,661,153",fc="#DCE8F3")
box(54,71,42,14,"Excluded (n = 98,115)\nInvalid sex: 17,019\nMissing age after correction: 81,096\n(potentially rare: 13,176)")
box(X,65,W,12,"Discharges with valid age and sex\nn = 61,490,522\nPotentially rare: 1,647,977",fc="#DCE8F3")
box(54,53,42,9,"Not selected into the random sample\nn = 58,490,522")
box(X,46,W,13,"Random sample for the models\n(seed 20250918)\nn = 3,000,000\nPotentially rare: 80,488",fc="#DCE8F3")
box(54,33,42,12,"Outside the main comparison\n(chapters O and R–Z)\nn = 1,344,623\n(potentially rare: 616)")
box(X,28,W,12,"Main scenario (E6b)\nn = 1,655,377\nPotentially rare: 79,872",fc="#DCE8F3",bold=True)
box(4,5,28,14,"Potentially rare\nn = 79,872\nIn-hospital deaths:\n3,549 (4.44%)",fc="#FFFFFF",ec=ACC)
box(36,5,28,14,"Reference (ICD-10 A–N, P, Q,\nnot potentially rare)\nn = 1,575,505\nDeaths: 74,081 (4.70%)",fc="#FFFFFF",ec=ACC,fs=9.5)
arr(26,84,26,77.4); arr(48,78,54,78)
arr(26,65,26,59.4); arr(48,57.5,54,57.5)
arr(26,46,26,40.4); arr(48,39,54,39)
arr(18,28,18,19.4); arr(50,28,50,19.4)
for t,y in (("Source",None),): pass
ax.text(4,1.5,"Percentages are crude in-hospital mortality. Potentially rare: principal diagnosis classified as potentially rare (see Methods).",fontsize=8,color="#444",va="center")
plt.savefig("figS1_flow_en.jpeg",dpi=300,bbox_inches="tight",pil_kwargs={"quality":93}); print("ok")
