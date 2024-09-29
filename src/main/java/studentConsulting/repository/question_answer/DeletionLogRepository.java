package studentConsulting.repository.question_answer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;

import java.util.Optional;

@Repository
public interface DeletionLogRepository extends PagingAndSortingRepository<DeletionLogEntity, Integer>, JpaSpecificationExecutor<DeletionLogEntity> {
    Optional<DeletionLogEntity> findByQuestionId(Integer questionId);

    @Query("SELECT d FROM DeletionLogEntity d " +
            "WHERE d.question.department.id = :departmentId")
    Page<DeletionLogEntity> findAllByDepartmentId(@Param("departmentId") Integer departmentId, Pageable pageable);

    @Query("SELECT d FROM DeletionLogEntity d WHERE d.deletedBy = CONCAT(:lastName, ' ', :firstName)")
    Page<DeletionLogEntity> findAllByConsultantFullName(@Param("firstName") String firstName, @Param("lastName") String lastName, Pageable pageable);

}

