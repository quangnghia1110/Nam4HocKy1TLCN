package studentConsulting.repository.feedback;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.feedback.RatingEntity;

public interface RatingRepository extends  JpaRepository<RatingEntity, Integer>{

}
