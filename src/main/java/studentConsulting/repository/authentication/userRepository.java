package studentConsulting.repository.authentication;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.entity.authentication.accountEntity;
import studentConsulting.entity.authentication.userEntity;

import java.util.Optional;

@Repository
public interface userRepository extends JpaRepository<userEntity, Long> {

    @Query("SELECT u FROM userEntity u WHERE u.accountModel=:account")
    userEntity findUserInfoModelByAccountModel(@Param("account") accountEntity accountModel);

    @Query("SELECT u FROM userEntity u WHERE u.id=:id")
    Optional<userEntity> findById(@Param("id") Long id);

    @Query("SELECT u FROM userEntity u WHERE u.accountModel.roleModel.name = :rolename")
    Iterable<userEntity> findAllByRoleName(@Param("rolename") String rolename);
}
