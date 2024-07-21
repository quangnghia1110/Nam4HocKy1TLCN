package studentConsulting.repository.news;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.news.NewsImageEntity;

public interface NewsImageRepository extends  JpaRepository<NewsImageEntity, Integer>{

}
