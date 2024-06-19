package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.entity.authentication.accountEntity;

@Repository
public interface accountRepository extends JpaRepository<accountEntity, Long> {

    @Query("SELECT u FROM accountEntity u WHERE u.username=:username")
    accountEntity findAccountByUsername(@Param("username") String username);

    @Query("SELECT u FROM accountEntity u WHERE u.email=:email")
    accountEntity findAccountByEmail(@Param("email") String email);

    boolean existsByEmail(String email);

    @Query("SELECT u FROM accountEntity u WHERE u.verifyRegister= :verifyRegister")
    accountEntity findByVerifyCode(@Param("verifyRegister") String verifyRegister);
}
