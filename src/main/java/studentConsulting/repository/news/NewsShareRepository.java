package studentConsulting.repository.news;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.news.NewsShareEntity;

public interface NewsShareRepository extends  JpaRepository<NewsShareEntity, Integer>{

}
