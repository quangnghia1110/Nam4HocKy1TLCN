package studentConsulting.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;

@Repository
public interface UserRepository extends JpaRepository<UserInformationEntity, Integer>, JpaSpecificationExecutor<UserInformationEntity> {

	@Query("SELECT u FROM UserInformationEntity u WHERE u.account=:account")
	UserInformationEntity findUserInfoModelByAccountModel(@Param("account") AccountEntity accountModel);

	@Query("SELECT u FROM UserInformationEntity u WHERE u.id=:id")
	Optional<UserInformationEntity> findById(@Param("id") Integer integer);

	// Iterator hỗ trợ forEach, không tải lên toàn bộ và k yêu câu lưu trữ
	@Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :rolename")
	Iterable<UserInformationEntity> findAllByRoleName(@Param("rolename") String rolename);

	boolean existsByPhone(String phone);

	boolean existsByStudentCode(String studentCode);

	boolean existsByAccount_Email(String email);

	Optional<UserInformationEntity> findByAccount_Username(String username);

	Optional<UserInformationEntity> findByStudentCode(String studentCode);

	@Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :roleName")
	Page<UserInformationEntity> findAllByRoleName(@Param("roleName") String roleName, Pageable pageable);

	@Query("SELECT u FROM UserInformationEntity u WHERE u.account.role.name = :roleName AND u.account.department.id = :departmentId")
	Page<UserInformationEntity> findAllByRoleNameAndDepartment(@Param("roleName") String roleName, @Param("departmentId") Integer departmentId, Pageable pageable);

	@Query("SELECT u FROM UserInformationEntity u WHERE LOWER(u.firstName) LIKE LOWER(CONCAT('%', :firstName, '%'))AND u.account.role.name = :roleName")
	Page<UserInformationEntity> findByFirstNameAndRoleName(@Param("firstName") String firstName, @Param("roleName") String roleName, Pageable pageable);

	@Query("SELECT u FROM UserInformationEntity u WHERE u.account.department.id = :departmentId AND LOWER(u.firstName) LIKE LOWER(CONCAT('%', :firstName, '%')) AND u.account.role.name = :roleName")
    Page<UserInformationEntity> findByDepartmentAndFirstNameAndRoleName(@Param("departmentId") Integer departmentId, @Param("firstName") String firstName, @Param("roleName") String roleName, Pageable pageable);

	@Query("SELECT u FROM UserInformationEntity u JOIN u.account a WHERE a.role.name = :roleName AND a.department = :department")
	List<UserInformationEntity> findByDepartmentAndRoleName(@Param("department") DepartmentEntity department, @Param("roleName") String roleName);

	
	@Query("SELECT u FROM UserInformationEntity u WHERE u.account.username = :username")
    Optional<UserInformationEntity> findByAccountUsername(@Param("username") String username);

	@Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END " +
	           "FROM UserInformationEntity u " +
	           "JOIN u.account a " +
	           "JOIN a.role r " +
	           "WHERE u.id = :userId AND r.name = :roleName")
	    boolean existsByUserIdAndRoleName(@Param("userId") Integer userId, @Param("roleName") String roleName);
}
