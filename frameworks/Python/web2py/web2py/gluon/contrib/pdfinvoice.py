"""
BSD license -  created by Massimo Di Pierro
"""
from reportlab.pdfgen.canvas import Canvas
from reportlab.platypus import Table
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from decimal import Decimal
import cStringIO
import datetime

def listify(item):
    if isinstance(item,basestring):
        item = item.split('\n')
    return item

class PDF(object):
    def __init__(self, page_size=A4, font_face='Helvetica'):
        self.page_size = page_size
        self.font_face = font_face
        self.logo = None
    def format_currency(self,value):
        a = list(str(int(value)))
        for k in range(len(a)-3,0,-3):
            a.insert(k,',')
        a = ''.join(a)
        b = ("%.2f" % (value-int(value)))[2:]
        return "%s.%s" % (a,b)
    def draw(self, invoice, items_page=10):
        """ Draws the invoice """
        buffer = cStringIO.StringIO()
        invoice_items = invoice['items']
        pages = max((len(invoice_items)-2)/items_page+1,1)
        canvas = Canvas(buffer, pagesize=self.page_size)
        for page in range(pages):
            canvas.translate(0, 29.7 * cm)
            canvas.setFont(self.font_face, 10)

            canvas.saveState()
            canvas.setStrokeColorRGB(0.9, 0.5, 0.2)
            canvas.setFillColorRGB(0.2, 0.2, 0.2)
            canvas.setFont(self.font_face, 16)
            canvas.drawString(1 * cm, -1 * cm, invoice.get('title',''))
            if self.logo:
                canvas.drawInlineImage(self.logo, 1 * cm, -1 * cm, 250, 16)
            canvas.setLineWidth(4)
            canvas.line(0, -1.25 * cm, 21.7 * cm, -1.25 * cm)
            canvas.restoreState()

            canvas.saveState()
            notes = listify(invoice.get('notes',''))
            textobject = canvas.beginText(1 * cm, -25 * cm)
            for line in notes:
                textobject.textLine(line)
            canvas.drawText(textobject)
            textobject = canvas.beginText(18 * cm, -28 * cm)
            textobject.textLine('Pag.%s/%s' % (page+1,pages))
            canvas.drawText(textobject)
            canvas.restoreState()

            canvas.saveState()
            business_details = listify(invoice.get('from','FROM:'))
            canvas.setFont(self.font_face, 9)
            textobject = canvas.beginText(13 * cm, -2.5 * cm)
            for line in business_details:
                textobject.textLine(line)
            canvas.drawText(textobject)
            canvas.restoreState()

            canvas.saveState()
            client_info = listify(invoice.get('to','TO:'))
            textobject = canvas.beginText(1.5 * cm, -2.5 * cm)
            for line in client_info:
                textobject.textLine(line)
            canvas.drawText(textobject)
            canvas.restoreState()

            textobject = canvas.beginText(1.5 * cm, -6.75 * cm)
            textobject.textLine(u'Invoice ID: %s' % invoice.get('id','<invoice id>'))
            textobject.textLine(u'Invoice Date: %s' % invoice.get('date',datetime.date.today()))
            textobject.textLine(u'Client: %s' % invoice.get('client_name','<invoice client>'))
            canvas.drawText(textobject)

            items = invoice_items[1:][page*items_page:(page+1)*items_page]
            if items:
                data = [invoice_items[0]]
                for item in items:
                    data.append([
                            self.format_currency(x)
                            if isinstance(x,float) else x
                            for x in item])
                righta = [k for k,v in enumerate(items[0])
                          if isinstance(v,(int,float,Decimal))]
                if page == pages-1:
                    total = self.format_currency(invoice['total'])
                else:
                    total = ''
                data.append(['']*(len(items[0])-1)+[total])
                colWidths = [2.5*cm]*len(items[0])
                colWidths[1] = (21.5-2.5*len(items[0]))*cm
                table = Table(data, colWidths=colWidths)
                table.setStyle([
                        ('FONT', (0, 0), (-1, -1), self.font_face),
                        ('FONTSIZE', (0, 0), (-1, -1), 8),
                        ('TEXTCOLOR', (0, 0), (-1, -1), (0.2, 0.2, 0.2)),
                        ('GRID', (0, 0), (-1, -2), 1, (0.7, 0.7, 0.7)),
                        ('GRID', (-1, -1), (-1, -1), 1, (0.7, 0.7, 0.7)),
                        ('BACKGROUND', (0, 0), (-1, 0), (0.8, 0.8, 0.8)),
                        ]+[('ALIGN',(k,0),(k,-1),'RIGHT') for k in righta])
                tw, th, = table.wrapOn(canvas, 15 * cm, 19 * cm)
                table.drawOn(canvas, 1 * cm, -8 * cm - th)

            if page == pages-1:
                items = invoice['totals'][1:]
                if items:
                    data = [invoice['totals'][0]]
                    for item in items:
                        data.append([
                                self.format_currency(x)
                                if isinstance(x,float) else x
                                for x in item])
                    righta = [k for k,v in enumerate(items[0])
                              if isinstance(v,(int,float,Decimal))]
                    total = self.format_currency(invoice['total'])
                    data.append(['']*(len(items[0])-1)+[total])
                    colWidths = [2.5*cm]*len(items[0])
                    colWidths[1] = (21.5-2.5*len(items[0]))*cm
                    table = Table(data, colWidths=colWidths)
                    table.setStyle([
                            ('FONT', (0, 0), (-1, -1), self.font_face),
                            ('FONTSIZE', (0, 0), (-1, -1), 8),
                            ('TEXTCOLOR', (0, 0), (-1, -1), (0.2, 0.2, 0.2)),
                            ('GRID', (0, 0), (-1, -2), 1, (0.7, 0.7, 0.7)),
                            ('GRID', (-1, -1), (-1, -1), 1, (0.7, 0.7, 0.7)),
                            ('BACKGROUND', (0, 0), (-1, 0), (0.8, 0.8, 0.8)),
                            ]+[('ALIGN',(k,0),(k,-1),'RIGHT') for k in righta])
                    tw, th, = table.wrapOn(canvas, 15 * cm, 19 * cm)
                    table.drawOn(canvas, 1 * cm, -18 * cm - th)
            canvas.showPage()
            canvas.save()
        return buffer.getvalue()

if __name__=='__main__':
    invoice = {
        'title':          'Invoice - web2py.com',
        'id':             '00001',
        'date':           '10/10/2013',
        'client_name':    'Nobody',
        'from':           'FROM:\nweb2py.com\nWabash ave\nChicago',
        'to':             'TO:\nNobody\nHis address',
        'notes':          'no comment!',
        'total':          650.00,
        'items': [
            ['Codice','Desc','Quantity','Unit price','Total']]+[
            ['000001','Chair',2,10.0,20.0] for k in range(30)],
        'totals': [
            ['Codice','Desc','Total']]+[
            ['000001','Chairs',600.0],
            ['','Tax',50.0]],
        }
    print PDF().draw(invoice,items_page=20)
