package studentConsulting.repository.admin;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.AccountEntity;

import java.util.Optional;
import java.util.Set;

@Repository
public interface AccountRepository extends PagingAndSortingRepository<AccountEntity, Integer>, JpaSpecificationExecutor<AccountEntity>, JpaRepository<AccountEntity, Integer> {

    Optional<AccountEntity> findByEmail(String email);

    @Query("SELECT u FROM AccountEntity u WHERE u.username=:username")
    AccountEntity findAccountByUsername(@Param("username") String username);

    @Query("SELECT u FROM AccountEntity u WHERE u.email=:email")
    AccountEntity findAccountByEmail(@Param("email") String email);

    boolean existsByEmail(String email);

    boolean existsByUsername(String username);

    Set<AccountEntity> findAllByIsOnline(Boolean isOnline);

}
