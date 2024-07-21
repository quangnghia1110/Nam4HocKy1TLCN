package studentConsulting.repository.news;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.news.NewsAttachmentEntity;

public interface NewsAttachmentRepository extends  JpaRepository<NewsAttachmentEntity, Integer>{

}
