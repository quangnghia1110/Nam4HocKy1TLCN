package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;

@Repository
public interface ForwardQuestionRepository extends PagingAndSortingRepository<ForwardQuestionEntity, Integer>, JpaSpecificationExecutor<ForwardQuestionEntity> {

}
