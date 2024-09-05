package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.AccountEntity;

@Repository
public interface AccountRepository extends JpaRepository<AccountEntity, Long> {

	@Query("SELECT u FROM AccountEntity u WHERE u.username=:username")
	AccountEntity findAccountByUsername(@Param("username") String username);

	@Query("SELECT a FROM AccountEntity a LEFT JOIN FETCH a.role WHERE a.username = :username")
	AccountEntity findAccountWithRolesByUsername(@Param("username") String username);


	@Query("SELECT u FROM AccountEntity u WHERE u.email=:email")
	AccountEntity findAccountByEmail(@Param("email") String email);

	boolean existsByEmail(String email);
	
	boolean existsByUsername(String username);

	@Query("SELECT u FROM AccountEntity u WHERE u.verifyRegister= :verifyRegister")
	AccountEntity findByVerifyCode(@Param("verifyRegister") String verifyRegister);
}
