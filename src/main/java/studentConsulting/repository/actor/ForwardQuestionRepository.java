package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.ForwardQuestionEntity;

import java.util.Optional;

@Repository
public interface ForwardQuestionRepository extends PagingAndSortingRepository<ForwardQuestionEntity, Integer>, JpaSpecificationExecutor<ForwardQuestionEntity> {
    @Query("SELECT f FROM ForwardQuestionEntity f WHERE f.id = :forwardQuestionId AND (f.fromDepartment.id = :departmentId OR f.toDepartment.id = :departmentId)")
    Optional<ForwardQuestionEntity> findByIdAndDepartmentId(@Param("forwardQuestionId") Integer forwardQuestionId, @Param("departmentId") Integer departmentId);

}
