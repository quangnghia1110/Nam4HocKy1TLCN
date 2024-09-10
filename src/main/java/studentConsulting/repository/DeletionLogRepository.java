package studentConsulting.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;

@Repository
public interface DeletionLogRepository extends PagingAndSortingRepository<DeletionLogEntity, Integer>, JpaSpecificationExecutor<DeletionLogEntity> {
	Optional<DeletionLogEntity> findByQuestionId(Integer questionId);

}

