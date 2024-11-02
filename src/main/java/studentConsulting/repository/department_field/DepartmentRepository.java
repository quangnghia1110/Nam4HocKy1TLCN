package studentConsulting.repository.department_field;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import studentConsulting.model.entity.department_field.DepartmentEntity;

import java.util.List;
import java.util.Optional;

public interface DepartmentRepository extends PagingAndSortingRepository<DepartmentEntity, Integer>, JpaSpecificationExecutor<DepartmentEntity>, JpaRepository<DepartmentEntity, Integer> {
    @Query("SELECT d.id FROM DepartmentEntity d JOIN d.accounts a WHERE a.id = :advisorId")
    Integer findDepartmentsByManagerId(@Param("advisorId") Integer advisorId);

    @Query("SELECT d.id FROM UserInformationEntity u JOIN u.account a JOIN a.department d WHERE u.id = :userId")
    Integer findDepartmentByUserId(@Param("userId") Integer userId);

    Page<DepartmentEntity> findAllByNameContaining(String name, Pageable pageable);

    Optional<DepartmentEntity> findByName(String name);

    @Query("SELECT d.id FROM DepartmentEntity d")
    List<Integer> findAllDepartmentIds();
}
