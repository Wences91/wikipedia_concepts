library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(RColorBrewer)

options(scipen=10000)

# Concept analysis
concepts <- read.delim('data/openalex_concepts.tsv')
concepts_stats <- concepts %>%
  group_by(level) %>%
  summarise(concepts=n(),
            works_count=mean(works_count),
            cited_by_count=mean(cited_by_count))


concepts_metrics <- read.delim('data/concepts_metrics.tsv')
concepts_metrics_stats <- concepts_metrics %>%
  group_by(level) %>%
  summarise(concepts=n(),
            works_count=mean(works_count),
            cited_by_count=mean(cited_by_count),
            views=mean(views, na.rm=TRUE),
            references=mean(references, na.rm=TRUE))

cor(concepts_metrics[,3:8], method = 'spearman',use = 'complete.obs')

png('Figure_2.png', width=1100, height=700, res=120)
ggplot(data=concepts_metrics[which(concepts_metrics$level==0),], aes(x=works_count,
                                                                     y=views,
                                                                     #color=Color,
                                                                     size=cited_by_count,
                                                                     label=gsub('_', ' ', title)))+
  geom_point(colour='black', pch=21, fill='#edb066', alpha=0.75)+
  geom_text_repel(aes(size=0.2*cited_by_count),
                  box.padding = 0.5, min.segment.length = 1,# max.overlaps = Inf,
                  color='black',
                  show.legend = FALSE)+
  scale_x_continuous(labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  labs(x='Works count', y='Wikipedia views')+
  scale_radius(range = c(5,14), name = 'Cited by count', labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme_classic()+
  theme(legend.position="bottom",
        panel.grid.major = element_line(linetype = 2, linewidth=0.4, color='grey90'),
        axis.text.x=element_text(size=14, color='black'),
        axis.text.y=element_text(size=14, color='black'),
        axis.title.x=element_text(size=16, face= 'bold'),
        axis.title.y=element_text(size=16, face= 'bold'),
        legend.title=element_text(size=12, face= 'bold'),
        legend.text=element_text(size=11, color = 'black'))
dev.off()

concepts_schema <- read.delim('data/openalex_concepts_schema.tsv')
concepts_schema$openalex_id <- toupper(concepts_schema$openalex_id)
concepts_schema$openalex_id <- gsub('HTTPS://OPENALEX.ORG/', 'https://openalex.org/', concepts_schema$openalex_id)
concepts_schema$parent_ids <- toupper(concepts_schema$parent_ids)
concepts_schema$parent_ids <- gsub('HTTPS://OPENALEX.ORG/', 'https://openalex.org/', concepts_schema$parent_ids)
concepts_schema <- separate_rows(concepts_schema, parent_ids, sep = ', ')

all(concepts_schema$openalex_id %in% concepts$id)
all(concepts_schema$parent_ids[which(concepts_schema$parent_ids!='')] %in% concepts$id)

concepts_schema <- concepts_schema[which(concepts_schema$level %in% c(0,1,2)),]
length(unique(concepts_schema$openalex_id)) #21758
concepts_schema <- left_join(concepts_schema, concepts_schema[,c('openalex_id', 'display_name', 'level', 'parent_ids')], by=c('parent_ids'='openalex_id'))
concepts_schema <- left_join(concepts_schema, concepts_schema[,c('openalex_id', 'display_name.x', 'level.x')], by=c('parent_ids.y'='openalex_id'))
concepts_schema$display_name.x.y[which(is.na(concepts_schema$display_name.x.y))] <- concepts_schema$display_name.y[which(is.na(concepts_schema$display_name.x.y))]
concepts_schema$level.x.y[which(is.na(concepts_schema$level.x.y))] <- concepts_schema$level.y[which(is.na(concepts_schema$level.x.y))]

concepts_schema <- concepts_schema[, c('openalex_id', 'level.x.x', 'display_name.x.x', 'display_name.x.y', 'level.x.y')]
concepts_schema <- unique(concepts_schema)
length(unique(concepts_schema$openalex_id)) #21758
table(concepts_schema$level.x.y)
table(table(concepts_schema$openalex_id))

concepts_schema$display_name.x.x <- gsub(' ', '_', concepts_schema$display_name.x.x)
concepts_schema <- inner_join(concepts_schema, concepts_metrics, by=c('display_name.x.x'='title', 'level.x.x'='level'))

concepts_schema <- concepts_schema %>%
  group_by(display_name.x.y) %>%
  summarise(concepts = n(),
            works_count = median(works_count),
            views = median(views),
            cited_by_count = median(cited_by_count))


png('Figure_3.png', width=1100, height=750, res=120)
ggplot(data=concepts_schema[which(!is.na(concepts_schema$display_name.x.y)),], aes(x=works_count,
                                 y=views,
                                 fill=concepts,
                                 size=cited_by_count,
                                 label=display_name.x.y))+
  geom_point(colour='black',pch=21, alpha=.9)+
  geom_text_repel(aes(size=0.2*cited_by_count),
                  box.padding = 0.35, min.segment.length = 1,# max.overlaps = Inf,
                  color='black',
                  show.legend = FALSE)+
  scale_x_continuous(labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(0,5000,1000))+
  labs(x='Works count (median)', y='Wikipedia views (median)')+
  scale_radius(range = c(5,14), name = 'Cited by count (median)', labels=function(x) format(x, big.mark = ',', scientific = FALSE))+
  guides(fill= guide_colorbar(barwidth = 15, title = 'Concepts', title.vjust = 0.85, order=1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.box = 'vertical',
        legend.margin = margin(0,0,0,0),
        panel.grid.major = element_line(linetype = 2, linewidth=0.4, color='grey90'),
        axis.text.x=element_text(size=14, color='black'),
        axis.text.y=element_text(size=14, color='black'),
        axis.title.x=element_text(size=16, face= 'bold'),
        axis.title.y=element_text(size=16, face= 'bold'),
        legend.title=element_text(size=12, face= 'bold'),
        legend.text=element_text(size=11, color = 'black'))
dev.off()

concepts_metrics_cor <- data.frame()
for(i in unique(concepts_metrics$level)){
  aux<-data.frame(level=i,
             cor=round(cor.test(concepts_metrics[concepts_metrics$level==i,'works_count'],
                                concepts_metrics[concepts_metrics$level==i,'views'],
                                method = 'spearman',use = 'complete.obs')$estimate, 3),
             p.value=round(cor.test(concepts_metrics[concepts_metrics$level==i,'works_count'],
                                   concepts_metrics[concepts_metrics$level==i,'views'],
                                   method = 'spearman',use = 'complete.obs')$p.value, 3))
  concepts_metrics_cor <<- rbind.data.frame(concepts_metrics_cor,
                                           aux,
                                           stringsAsFactors = FALSE)
  
}

concepts_metrics$level_name <- concepts_metrics$level
for(i in unique(concepts_metrics$level)){
  concepts_metrics$level_name[which(concepts_metrics$level_name==i)] <- paste0(c('atop(bold(Level)~bold("', i ,'"),~',
                                                                                 'ρ == ', concepts_metrics_cor$cor[concepts_metrics_cor$level==i],
                                                                                 '~"p-value" == ', concepts_metrics_cor$p.value[concepts_metrics_cor$level==i],')'
                                                                                 ),
                                                                               sep='',collapse = '')
  
}

png('Figure_1.png', width=1100, height=800, res=120)
ggplot(data=concepts_metrics, aes(x=works_count+1,
                                  y=views+1,
                                  color=cited_by_count+1,
                                  label=title))+
  geom_point(alpha=.5, stroke=0)+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_color_gradient2(low='white', high='red', trans='log10',
                        breaks = scales::trans_breaks("log10", function(x) 10^x),
                        labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(x='Works count (log10)', y='Wikipedia views (log10)', color='Cited by count (log10)')+
  guides(colour = guide_colorbar(barwidth = 12, title.vjust = 0.75))+
  annotation_logticks() +
  theme_classic()+
  theme(legend.position="bottom",
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        panel.grid.major = element_line(linetype = 2, linewidth=0.4, color='grey90'),
        axis.text.x=element_text(size=11, color='black'),
        axis.text.y=element_text(size=11, color='black'),
        axis.title.x=element_text(size=16, face= 'bold'),
        axis.title.y=element_text(size=16, face= 'bold'),
        legend.title=element_text(size=12, color='black'),
        legend.text=element_text(size=11, color='black'))+
  facet_wrap(.~level_name, scales = 'free', labeller = label_parsed)
dev.off()
