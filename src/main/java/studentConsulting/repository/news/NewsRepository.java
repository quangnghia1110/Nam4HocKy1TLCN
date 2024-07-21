package studentConsulting.repository.news;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.news.NewsEntity;

public interface NewsRepository extends  JpaRepository<NewsEntity, Integer>{

}
