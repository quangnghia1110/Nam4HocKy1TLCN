package studentConsulting.repository.feedback;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.feedback.ReviewEntity;

public interface ReviewRepository extends  JpaRepository<ReviewEntity, Integer>{

}
