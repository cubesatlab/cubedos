from address import Address
from args import Args
from cubedos_time import CubedOSTime


class Command:

    def __init__(self, addr: Address, args:Args, time: CubedOSTime):
        """Creates a new Command

        Args:
            addr (Address): The address that the command is addressed to
            args (Args): The arguments to the command
            time (CubedOSTime): The time to commit the command.
        """
        pass

    def change_args(self, args: Args):
        """Method to change the arguments to a command

        Args:
            args (Args): The arguments to replace the old arguments
        """
        pass

    def change_time(self, time: CubedOSTime):
        """Changes the time that the command will be run

        Args:
            time (CubedOSTime): The new time to run the message
        """
        pass
    
    def change_address(self, address: Address):
        """Changes the address that the command will be sent to

        Args:
            address (Address): The new address to send the command to
        """
        pass

    def get_args(self) -> Args:
        """Returns the arguments to be used while running the command

        Returns:
            Args: The arguments that the command will run with
        """
        pass

    def get_address(self) -> Address:
        """Returns the address that the command will be sent to

        Returns:
            Address: The address that the command will be sent to
        """
        pass

    def get_time(self) -> CubedOSTime:
        """returns the time that the command will be run at

        Returns:
            CubedOSTime: The tme that the command will be run at
        """
        pass

    def __str__(self) -> str:
        """generates a human-readable string representation of the command

        Returns:
            str: The human-readable string
        """
        pass

    def __hex__(self) -> str:
        """returns a machine-readable string representation of the command

        Returns:
            str: The machine-readable string
        """
        pass

    def __eq__(self, __o: Command) -> bool:
        """Compares to another Command. if Address, Time, and Args are all matching, then the two objects are equal

        Args:
            __o (Command): The other Command to be checked against

        Returns:
            bool: If the two Commands are equal
        """
        pass

    